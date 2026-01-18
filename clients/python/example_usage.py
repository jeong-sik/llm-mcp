#!/usr/bin/env python3
"""
Compact Protocol v1.3 - Python Client Usage Example

실제 llm-mcp 서버와 통신하는 예시입니다.
서버 시작: cd ~/me/features/llm-mcp && dune exec llm-mcp -- --port 8932
"""

import requests
from compact_decoder import (
    decode,
    decode_base85,
    create_delta_state,
    apply_delta,
    CompactResponse,
)


# === Configuration ===
LLM_MCP_URL = "http://localhost:8932/mcp"


def call_llm(
    tool: str,
    prompt: str,
    response_format: str = "compact",
    **kwargs
) -> CompactResponse:
    """llm-mcp를 통해 LLM을 호출합니다."""

    payload = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "tools/call",
        "params": {
            "name": tool,
            "arguments": {
                "prompt": prompt,
                "response_format": response_format,
                **kwargs
            }
        }
    }

    response = requests.post(LLM_MCP_URL, json=payload, timeout=300)
    response.raise_for_status()

    result = response.json()

    # MCP 응답에서 텍스트 추출
    if "result" in result and "content" in result["result"]:
        raw_text = result["result"]["content"][0]["text"]
        return decode(raw_text)
    elif "error" in result:
        raise Exception(f"MCP Error: {result['error']}")
    else:
        raise Exception(f"Unexpected response: {result}")


def example_basic_call():
    """기본 호출 예시"""
    print("=== Basic Call (Compact DSL) ===")

    result = call_llm(
        tool="gemini",
        prompt="What is 2+2? Reply with just the number.",
        response_format="compact"
    )

    print(f"Model: {result.model}")
    print(f"Status: {result.status}")
    print(f"Tokens: {result.tokens}")
    print(f"Result: {result.result}")
    print()


def example_verbose_vs_compact():
    """Verbose vs Compact 비교"""
    print("=== Verbose vs Compact Comparison ===")

    prompt = "Say hello in Korean"

    # Verbose
    verbose_result = call_llm("gemini", prompt, response_format="verbose")
    print(f"Verbose result: {verbose_result.result}")

    # Compact
    compact_result = call_llm("gemini", prompt, response_format="compact")
    print(f"Compact result: {compact_result.result}")
    print()


def example_different_llms():
    """다양한 LLM 호출 (MAGI Pentarchy)"""
    print("=== MAGI Pentarchy Examples ===")

    prompt = "What is functional programming? One sentence."

    llms = [
        ("gemini", "CASPER (전략가)"),
        ("codex", "MELCHIOR (과학자)"),
        # ("claude-cli", "BALTHASAR (거울)"),  # 자기 자신 호출은 skip
        # ("ollama", "Local LLM"),  # Ollama 실행 필요
    ]

    for tool, name in llms:
        try:
            result = call_llm(tool, prompt, response_format="compact")
            print(f"{name}: {result.result[:80]}...")
        except Exception as e:
            print(f"{name}: Error - {e}")
    print()


def example_streaming_delta():
    """스트리밍 Delta 프로토콜 시뮬레이션"""
    print("=== Streaming Delta Protocol ===")

    state = create_delta_state()

    # 실제로는 SSE나 WebSocket으로 받음
    # 여기서는 시뮬레이션
    deltas = [
        "D|F|The answer",
        "D|+| to life",
        "D|+|, the universe",
        "D|+|, and everything",
        "D|+| is 42.",
    ]

    for delta in deltas:
        content = apply_delta(state, delta)
        print(f"  → {content}")

    print(f"\nFinal: {state.content}")
    print()


def example_base85_decode():
    """Base85 디코딩 예시"""
    print("=== Base85 Decoding ===")

    # OCaml encode_base85("hello") = "Xk#0@Zv"
    encoded = "Xk#0@Zv"
    decoded = decode_base85(encoded)

    print(f"Encoded: {encoded}")
    print(f"Decoded: {decoded}")  # b'hello'
    print(f"As string: {decoded.decode('utf-8')}")
    print()


def example_auto_format():
    """Auto 포맷 (자동 최적 선택)"""
    print("=== Auto Format Selection ===")

    # 짧은 응답 → Compact DSL 선택됨
    short_result = call_llm(
        "gemini",
        "Say 'hi'",
        response_format="auto"
    )
    print(f"Short response: {short_result.result}")

    # 긴 응답 → Compressed 선택됨 (서버가 자동 판단)
    # long_result = call_llm(
    #     "gemini",
    #     "Write a 500 word essay about AI",
    #     response_format="auto"
    # )
    print()


def check_server():
    """서버 상태 확인"""
    try:
        response = requests.get("http://localhost:8932/health", timeout=5)
        if response.status_code == 200:
            print("✅ llm-mcp server is running")
            return True
    except:
        pass

    print("❌ llm-mcp server is not running")
    print("   Start with: cd ~/me/features/llm-mcp && dune exec llm-mcp -- --port 8932")
    return False


if __name__ == "__main__":
    print("Compact Protocol v1.3 - Python Client Examples\n")

    # 서버 없이 실행 가능한 예시
    example_base85_decode()
    example_streaming_delta()

    # 서버 필요한 예시
    if check_server():
        print()
        example_basic_call()
        # example_verbose_vs_compact()
        # example_different_llms()
        # example_auto_format()
