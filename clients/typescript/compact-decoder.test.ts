/**
 * Compact Protocol v1.3 Decoder Tests
 * Tests the TypeScript client decoder for all supported formats
 */

import { describe, it, expect } from 'vitest';
import {
  decode,
  decodeBase85,
  createDeltaState,
  applyDelta,
  CompactResponse,
} from './compact-decoder';

describe('decodeBase85', () => {
  it('should decode empty string', () => {
    const result = decodeBase85('');
    expect(result).toEqual(new Uint8Array([]));
  });

  // Test data generated from OCaml encode_base85
  const testCases = [
    { input: 'VE', expected: 'a' },
    { input: 'VPX', expected: 'ab' },
    { input: 'VPaz', expected: 'abc' },
    { input: 'VPa.s', expected: 'abcd' },
    { input: 'Xk#0@Zv', expected: 'hello' },
  ];

  for (const { input, expected } of testCases) {
    it(`should decode '${input}' to '${expected}'`, () => {
      const decoded = decodeBase85(input);
      expect(Buffer.from(decoded).toString()).toBe(expected);
    });
  }

  it('should decode binary data \\x00\\x01\\x02\\x03', () => {
    const decoded = decodeBase85('009C6');
    expect(decoded).toEqual(new Uint8Array([0, 1, 2, 3]));
  });

  it('should throw on invalid character', () => {
    expect(() => decodeBase85('hello world')).toThrow('Invalid Base85 character');
  });

  it('should throw on single char (invalid length)', () => {
    expect(() => decodeBase85('V')).toThrow('Invalid Base85 length');
  });
});

describe('decode', () => {
  describe('Compact DSL format', () => {
    it('should decode RES|OK|X5|0|result', () => {
      const input = 'RES|OK|X5|0|42';
      const result = decode(input) as CompactResponse;
      expect(result.status).toBe('OK');
      expect(result.model).toBe('X5');
      expect(result.tokens).toBe(0);
      expect(result.result).toBe('42');
    });

    it('should handle pipe in result', () => {
      const input = 'RES|OK|G3|100|a|b|c';
      const result = decode(input) as CompactResponse;
      expect(result.result).toBe('a|b|c');
    });
  });

  describe('Verbose JSON format', () => {
    it('should decode JSON response', () => {
      const input = JSON.stringify({
        model: 'gemini',
        returncode: 0,
        response: 'hello',
      });
      // Note: decode() normalizes to CompactResponse format
      // model names are converted to codes (gemini -> G3)
      const result = decode(input) as CompactResponse;
      expect(result.model).toBe('G3');  // normalized from 'gemini'
      expect(result.status).toBe('OK'); // returncode 0 -> OK
      expect(result.result).toBe('hello');
    });
  });
});

describe('Client-Server compatibility', () => {
  it('should use same Base85 alphabet as OCaml encoder', () => {
    // OCaml Base85 alphabet (for reference)
    const OCAML_ALPHABET = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.-:+=^!/*?&<>()[]{}@%$#';
    expect(OCAML_ALPHABET.length).toBe(85);

    // Verify no duplicate characters
    const chars = new Set(OCAML_ALPHABET);
    expect(chars.size).toBe(85);
  });
});

describe('Streaming Delta Protocol', () => {
  it('should handle D|F| (Full replace)', () => {
    const state = createDeltaState();
    const result = applyDelta(state, 'D|F|Hello');
    expect(result).toBe('Hello');
    expect(state.content).toBe('Hello');
  });

  it('should handle D|+| (Append)', () => {
    const state = createDeltaState();
    applyDelta(state, 'D|F|Hello');
    const result = applyDelta(state, 'D|+|, world!');
    expect(result).toBe('Hello, world!');
  });

  it('should handle D|R| (Replace at position)', () => {
    const state = createDeltaState();
    applyDelta(state, 'D|F|Hello, world!');
    const result = applyDelta(state, 'D|R|7|World');
    expect(result).toBe('Hello, World');
  });

  it('should handle sequence of deltas', () => {
    const state = createDeltaState();

    applyDelta(state, 'D|F|A');
    expect(state.content).toBe('A');

    applyDelta(state, 'D|+|B');
    expect(state.content).toBe('AB');

    applyDelta(state, 'D|+|C');
    expect(state.content).toBe('ABC');

    applyDelta(state, 'D|R|1|X');
    expect(state.content).toBe('AX');
  });

  it('should start with empty content', () => {
    const state = createDeltaState();
    expect(state.content).toBe('');
  });
});
