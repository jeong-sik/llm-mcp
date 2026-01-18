"""
Compact Protocol v1.3 Decoder Tests
Tests the Python client decoder for all supported formats
"""

import json
import pytest
from compact_decoder import (
    decode,
    decode_base85,
    create_delta_state,
    apply_delta,
    CompactResponse,
    Status,
    ModelCode,
    BASE85_ALPHABET,
)


class TestDecodeBase85:
    def test_empty_string(self):
        result = decode_base85("")
        assert result == b""

    # Test data generated from OCaml encode_base85
    @pytest.mark.parametrize(
        "encoded,expected",
        [
            ("VE", b"a"),
            ("VPX", b"ab"),
            ("VPaz", b"abc"),
            ("VPa.s", b"abcd"),
            ("Xk#0@Zv", b"hello"),
        ],
    )
    def test_ocaml_compatibility(self, encoded, expected):
        decoded = decode_base85(encoded)
        assert decoded == expected

    def test_binary_data(self):
        # \x00\x01\x02\x03 encoded by OCaml
        decoded = decode_base85("009C6")
        assert decoded == bytes([0, 1, 2, 3])

    def test_invalid_character(self):
        with pytest.raises(ValueError, match="Invalid Base85 character"):
            decode_base85("hello world")

    def test_invalid_length(self):
        with pytest.raises(ValueError, match="Invalid Base85 length"):
            decode_base85("V")


class TestDecode:
    def test_compact_dsl(self):
        result = decode("RES|OK|X5|0|42")
        assert isinstance(result, CompactResponse)
        assert result.status == Status.OK
        assert result.model == "X5"
        assert result.tokens == 0
        assert result.result == "42"

    def test_compact_dsl_pipe_in_result(self):
        result = decode("RES|OK|G3|100|a|b|c")
        assert isinstance(result, CompactResponse)
        assert result.result == "a|b|c"

    def test_verbose_json(self):
        input_json = json.dumps(
            {"model": "gemini", "returncode": 0, "response": "hello"}
        )
        result = decode(input_json)
        # Note: decode() normalizes to CompactResponse format
        assert isinstance(result, CompactResponse)
        assert result.model == ModelCode.G3  # normalized from 'gemini'
        assert result.status == Status.OK  # returncode 0 -> OK
        assert result.result == "hello"


class TestClientServerCompatibility:
    def test_alphabet_length(self):
        assert len(BASE85_ALPHABET) == 85

    def test_no_duplicate_chars(self):
        assert len(set(BASE85_ALPHABET)) == 85

    def test_alphabet_matches_ocaml(self):
        # Must match OCaml's base85_alphabet in types.ml
        expected = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.-:+=^!/*?&<>()[]{}@%$#"
        assert BASE85_ALPHABET == expected


class TestStreamingDelta:
    def test_full_replace(self):
        state = create_delta_state()
        result = apply_delta(state, "D|F|Hello")
        assert result == "Hello"
        assert state.content == "Hello"

    def test_append(self):
        state = create_delta_state()
        apply_delta(state, "D|F|Hello")
        result = apply_delta(state, "D|+|, world!")
        assert result == "Hello, world!"

    def test_replace_at_position(self):
        state = create_delta_state()
        apply_delta(state, "D|F|Hello, world!")
        result = apply_delta(state, "D|R|7|World")
        assert result == "Hello, World"

    def test_sequence_of_deltas(self):
        state = create_delta_state()

        apply_delta(state, "D|F|A")
        assert state.content == "A"

        apply_delta(state, "D|+|B")
        assert state.content == "AB"

        apply_delta(state, "D|+|C")
        assert state.content == "ABC"

        apply_delta(state, "D|R|1|X")
        assert state.content == "AX"

    def test_empty_initial_state(self):
        state = create_delta_state()
        assert state.content == ""


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
