"""
Compact Protocol v1.3 Decoder for Python

Decodes LLM-MCP responses in all supported formats:
- Verbose (JSON)
- Compact DSL (RES|OK|X5|0|result)
- Binary (M prefix - Base64 MessagePack)
- Base85 (A prefix - ASCII85 MessagePack)
- Compressed (Z prefix - Zlib + Base85)
"""

import base64
import json
import zlib
from dataclasses import dataclass, field
from enum import Enum
from typing import Optional, Union

import msgpack

# ============================================================================
# Types
# ============================================================================


class Status(Enum):
    OK = "OK"
    ERR = "ERR"
    PART = "PART"
    STREAM = "STREAM"


class ModelCode(Enum):
    G3 = "G3"  # Gemini
    C4 = "C4"  # Claude
    X5 = "X5"  # Codex
    OL = "OL"  # Ollama
    AD = "AD"  # ADAM
    SE = "SE"  # SEELE


# Status int mappings (v2/v3)
STATUS_MAP = {
    0: Status.OK,
    1: Status.ERR,
    2: Status.PART,
    3: Status.STREAM,
}

# Model int mappings (v2/v3)
MODEL_MAP = {
    0: ModelCode.G3,
    1: ModelCode.C4,
    2: ModelCode.X5,
    3: ModelCode.OL,
    4: ModelCode.AD,
    5: ModelCode.SE,
}


@dataclass
class CompactResponse:
    """Decoded Compact Protocol response."""

    status: Status
    model: Union[ModelCode, str]
    tokens: int
    result: str


@dataclass
class VerboseResponse:
    """Raw verbose JSON response."""

    model: str
    returncode: int
    response: str
    reasoning_effort: Optional[str] = None
    ultrathink: Optional[bool] = None


# ============================================================================
# Base85 (ASCII85) Decoder
# ============================================================================


# Must match OCaml's base85_alphabet in types.ml
BASE85_ALPHABET = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.-:+=^!/*?&<>()[]{}@%$#"
BASE85_REVERSE = {c: i for i, c in enumerate(BASE85_ALPHABET)}


def decode_base85(encoded: str) -> bytes:
    """Decode Base85 encoded string to bytes (OCaml-compatible custom alphabet)."""
    if len(encoded) == 0:
        return b""

    result = bytearray()
    i = 0

    while i + 5 <= len(encoded):
        # Decode 5 chars -> 4 bytes
        try:
            c0 = BASE85_REVERSE[encoded[i]]
            c1 = BASE85_REVERSE[encoded[i + 1]]
            c2 = BASE85_REVERSE[encoded[i + 2]]
            c3 = BASE85_REVERSE[encoded[i + 3]]
            c4 = BASE85_REVERSE[encoded[i + 4]]
        except KeyError:
            raise ValueError("Invalid Base85 character")

        # value = c0*85^4 + c1*85^3 + c2*85^2 + c3*85 + c4
        value = c0 * 52200625 + c1 * 614125 + c2 * 7225 + c3 * 85 + c4

        result.append((value >> 24) & 0xFF)
        result.append((value >> 16) & 0xFF)
        result.append((value >> 8) & 0xFF)
        result.append(value & 0xFF)
        i += 5

    # Handle remaining chars (2-4) -> (1-3) bytes
    remaining = len(encoded) - i
    if remaining > 0:
        if remaining == 1:
            raise ValueError("Invalid Base85 length (1 char remaining)")

        # Pad with last char of alphabet (84) for decoding
        padded = [84, 84, 84, 84, 84]
        for j in range(remaining):
            try:
                padded[j] = BASE85_REVERSE[encoded[i + j]]
            except KeyError:
                raise ValueError("Invalid Base85 character")

        value = (
            padded[0] * 52200625
            + padded[1] * 614125
            + padded[2] * 7225
            + padded[3] * 85
            + padded[4]
        )

        # Output (remaining - 1) bytes
        out_bytes = remaining - 1
        if out_bytes >= 1:
            result.append((value >> 24) & 0xFF)
        if out_bytes >= 2:
            result.append((value >> 16) & 0xFF)
        if out_bytes >= 3:
            result.append((value >> 8) & 0xFF)

    return bytes(result)


# ============================================================================
# MessagePack Decoder with Version Detection
# ============================================================================


def decode_msgpack(data: bytes) -> CompactResponse:
    """Decode MessagePack data with version detection."""
    decoded = msgpack.unpackb(data, raw=False)

    if not isinstance(decoded, list) or len(decoded) < 4:
        raise ValueError("Invalid MessagePack structure")

    version = decoded[0]

    if version == 1:
        # v1: [1, "OK", "G3", 150, "result"]
        return CompactResponse(
            status=Status(decoded[1]),
            model=decoded[2],
            tokens=decoded[3],
            result=decoded[4],
        )
    elif version == 2:
        # v2: [2, 0, 0, 150, "result"] - int encoding
        return CompactResponse(
            status=STATUS_MAP.get(decoded[1], Status.OK),
            model=MODEL_MAP.get(decoded[2], ModelCode.G3),
            tokens=decoded[3],
            result=decoded[4],
        )
    elif version == 3:
        # v3: [3, 0, 0, "result"] or [3, 0, 0, 150, "result"]
        if len(decoded) == 4:
            # tokens=0, omitted
            return CompactResponse(
                status=STATUS_MAP.get(decoded[1], Status.OK),
                model=MODEL_MAP.get(decoded[2], ModelCode.G3),
                tokens=0,
                result=decoded[3],
            )
        else:
            return CompactResponse(
                status=STATUS_MAP.get(decoded[1], Status.OK),
                model=MODEL_MAP.get(decoded[2], ModelCode.G3),
                tokens=decoded[3],
                result=decoded[4],
            )

    raise ValueError(f"Unknown MessagePack version: {version}")


# ============================================================================
# Main Decoder
# ============================================================================


def decode(input_str: str) -> CompactResponse:
    """
    Decode a Compact Protocol response.

    Args:
        input_str: Raw response string from LLM-MCP

    Returns:
        CompactResponse object

    Examples:
        >>> decode("RES|OK|X5|0|Hello World")
        CompactResponse(status=Status.OK, model='X5', tokens=0, result='Hello World')

        >>> decode('{"model":"codex","returncode":0,"response":"Hello"}')
        CompactResponse(status=Status.OK, model=ModelCode.X5, tokens=0, result='Hello')
    """
    trimmed = input_str.strip()

    # Compressed (Zlib + Base85)
    if trimmed.startswith("Z"):
        base85 = trimmed[1:]
        compressed = decode_base85(base85)
        decompressed = zlib.decompress(compressed)
        return decode_msgpack(decompressed)

    # Base85 MessagePack
    if trimmed.startswith("A"):
        base85 = trimmed[1:]
        data = decode_base85(base85)
        return decode_msgpack(data)

    # Base64 MessagePack
    if trimmed.startswith("M"):
        base64_str = trimmed[1:]
        binary = base64.b64decode(base64_str)
        return decode_msgpack(binary)

    # Legacy prefixes (v1.2 compatibility)
    if trimmed.startswith("A85:"):
        base85 = trimmed[4:]
        data = decode_base85(base85)
        return decode_msgpack(data)

    if trimmed.startswith("MPK:"):
        base64_str = trimmed[4:]
        binary = base64.b64decode(base64_str)
        return decode_msgpack(binary)

    # Compact DSL: RES|OK|X5|0|result
    if trimmed.startswith("RES|"):
        parts = trimmed.split("|")
        if len(parts) < 5:
            raise ValueError("Invalid Compact DSL format")

        return CompactResponse(
            status=Status(parts[1]),
            model=parts[2],
            tokens=int(parts[3]),
            # Rejoin remaining parts (result may contain |)
            result="|".join(parts[4:]),
        )

    # Try JSON (Verbose format)
    try:
        data = json.loads(trimmed)
        return CompactResponse(
            status=Status.OK if data.get("returncode", 0) == 0 else Status.ERR,
            model=_extract_model_code(data.get("model", "")),
            tokens=0,  # Not in verbose format
            result=data.get("response", ""),
        )
    except json.JSONDecodeError:
        raise ValueError(f"Unknown response format: {trimmed[:50]}...")


def _extract_model_code(model: str) -> Union[ModelCode, str]:
    """Extract model code from full model name."""
    model_lower = model.lower()
    if "gemini" in model_lower:
        return ModelCode.G3
    if "claude" in model_lower:
        return ModelCode.C4
    if "codex" in model_lower or "gpt" in model_lower:
        return ModelCode.X5
    if "ollama" in model_lower:
        return ModelCode.OL
    if "adam" in model_lower:
        return ModelCode.AD
    if "seele" in model_lower:
        return ModelCode.SE
    return model[:2].upper() if len(model) >= 2 else model


# ============================================================================
# Delta Streaming Decoder
# ============================================================================


@dataclass
class DeltaState:
    """State for delta streaming."""

    content: str = field(default="")


def create_delta_state() -> DeltaState:
    """Create a new delta state."""
    return DeltaState()


def apply_delta(state: DeltaState, delta: str) -> str:
    """
    Apply a delta event to the state.

    Args:
        state: Current delta state
        delta: Delta string (D|F|..., D|+|..., D|R|pos|...)

    Returns:
        Updated content string
    """
    # Compressed delta (D|Z+|..., D|ZF|..., D|ZR|...)
    if delta.startswith("D|Z"):
        op = delta[3]  # + or F or R
        rest = delta[5:]  # Skip D|Zx|
        compressed = decode_base85(rest)
        decompressed = zlib.decompress(compressed).decode("utf-8")

        if op == "F":
            state.content = decompressed
        elif op == "+":
            state.content += decompressed
        elif op == "R":
            # Decompressed format: pos|content
            pipe_idx = decompressed.index("|")
            pos = int(decompressed[:pipe_idx])
            new_content = decompressed[pipe_idx + 1 :]
            state.content = state.content[:pos] + new_content

        return state.content

    # Full replace
    if delta.startswith("D|F|"):
        state.content = delta[4:]
        return state.content

    # Append
    if delta.startswith("D|+|"):
        state.content += delta[4:]
        return state.content

    # Replace at position
    if delta.startswith("D|R|"):
        rest = delta[4:]
        pipe_idx = rest.index("|")
        pos = int(rest[:pipe_idx])
        new_content = rest[pipe_idx + 1 :]
        state.content = state.content[:pos] + new_content
        return state.content

    raise ValueError(f"Unknown delta format: {delta}")


# ============================================================================
# CLI / Testing
# ============================================================================

if __name__ == "__main__":
    # Example usage
    test_cases = [
        'RES|OK|X5|0|TypeScript is better for large codebases.',
        '{"model":"codex (gpt-5.2)","returncode":0,"response":"Hello World"}',
    ]

    for test in test_cases:
        result = decode(test)
        print(f"Input: {test[:50]}...")
        print(f"  Status: {result.status}")
        print(f"  Model: {result.model}")
        print(f"  Tokens: {result.tokens}")
        print(f"  Result: {result.result[:50]}...")
        print()

    # Delta streaming example
    state = create_delta_state()
    deltas = [
        "D|F|The",
        "D|+| answer",
        "D|+| is",
        "D|+| 42",
    ]

    print("Delta streaming example:")
    for d in deltas:
        content = apply_delta(state, d)
        print(f"  {d} -> '{content}'")
