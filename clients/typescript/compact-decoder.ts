/**
 * Compact Protocol v1.3 Decoder for TypeScript
 *
 * Decodes LLM-MCP responses in all supported formats:
 * - Verbose (JSON)
 * - Compact DSL (RES|OK|X5|0|result)
 * - Binary (M prefix - Base64 MessagePack)
 * - Base85 (A prefix - ASCII85 MessagePack)
 * - Compressed (Z prefix - Zlib + Base85)
 */

import { decode as msgpackDecode } from '@msgpack/msgpack';
import pako from 'pako';

// ============================================================================
// Types
// ============================================================================

export type Status = 'OK' | 'ERR' | 'PART' | 'STREAM';
export type ModelCode = 'G3' | 'C4' | 'X5' | 'OL' | 'AD' | 'SE' | string;

export interface CompactResponse {
  status: Status;
  model: ModelCode;
  tokens: number;
  result: string;
}

export interface VerboseResponse {
  model: string;
  returncode: number;
  response: string;
  reasoning_effort?: string;
  ultrathink?: boolean;
}

// Status int mappings (v2/v3)
const STATUS_MAP: Record<number, Status> = {
  0: 'OK',
  1: 'ERR',
  2: 'PART',
  3: 'STREAM',
};

// Model int mappings (v2/v3)
const MODEL_MAP: Record<number, ModelCode> = {
  0: 'G3',  // Gemini
  1: 'C4',  // Claude
  2: 'X5',  // Codex
  3: 'OL',  // Ollama
  4: 'AD',  // ADAM
  5: 'SE',  // SEELE
};

// ============================================================================
// Base85 Decoder (OCaml-compatible custom alphabet)
// ============================================================================

// Must match OCaml's base85_alphabet in types.ml
const BASE85_ALPHABET = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.-:+=^!/*?&<>()[]{}@%$#';

// Build reverse lookup table
const BASE85_REVERSE: Record<string, number> = {};
for (let i = 0; i < BASE85_ALPHABET.length; i++) {
  BASE85_REVERSE[BASE85_ALPHABET[i]] = i;
}

export function decodeBase85(encoded: string): Uint8Array {
  if (encoded.length === 0) return new Uint8Array([]);

  const result: number[] = [];
  let i = 0;

  while (i + 5 <= encoded.length) {
    // Decode 5 chars -> 4 bytes
    const c0 = BASE85_REVERSE[encoded[i]];
    const c1 = BASE85_REVERSE[encoded[i + 1]];
    const c2 = BASE85_REVERSE[encoded[i + 2]];
    const c3 = BASE85_REVERSE[encoded[i + 3]];
    const c4 = BASE85_REVERSE[encoded[i + 4]];

    if (c0 === undefined || c1 === undefined || c2 === undefined ||
        c3 === undefined || c4 === undefined) {
      throw new Error('Invalid Base85 character');
    }

    // value = c0*85^4 + c1*85^3 + c2*85^2 + c3*85 + c4
    const value = c0 * 52200625 + c1 * 614125 + c2 * 7225 + c3 * 85 + c4;

    result.push((value >> 24) & 0xff);
    result.push((value >> 16) & 0xff);
    result.push((value >> 8) & 0xff);
    result.push(value & 0xff);
    i += 5;
  }

  // Handle remaining chars (2-4) -> (1-3) bytes
  const remaining = encoded.length - i;
  if (remaining > 0) {
    if (remaining === 1) {
      throw new Error('Invalid Base85 length (1 char remaining)');
    }

    // Pad with last char of alphabet (84) for decoding
    const padded = [84, 84, 84, 84, 84];
    for (let j = 0; j < remaining; j++) {
      const c = BASE85_REVERSE[encoded[i + j]];
      if (c === undefined) {
        throw new Error('Invalid Base85 character');
      }
      padded[j] = c;
    }

    const value = padded[0] * 52200625 + padded[1] * 614125 +
                  padded[2] * 7225 + padded[3] * 85 + padded[4];

    // Output (remaining - 1) bytes
    const outBytes = remaining - 1;
    if (outBytes >= 1) result.push((value >> 24) & 0xff);
    if (outBytes >= 2) result.push((value >> 16) & 0xff);
    if (outBytes >= 3) result.push((value >> 8) & 0xff);
  }

  return new Uint8Array(result);
}

// ============================================================================
// MessagePack Decoder with Version Detection
// ============================================================================

function decodeMsgpack(data: Uint8Array): CompactResponse {
  const decoded = msgpackDecode(data) as unknown[];

  if (!Array.isArray(decoded) || decoded.length < 4) {
    throw new Error('Invalid MessagePack structure');
  }

  const version = decoded[0] as number;

  if (version === 1) {
    // v1: [1, "OK", "G3", 150, "result"]
    return {
      status: decoded[1] as Status,
      model: decoded[2] as ModelCode,
      tokens: decoded[3] as number,
      result: decoded[4] as string,
    };
  } else if (version === 2) {
    // v2: [2, 0, 0, 150, "result"] - int encoding
    return {
      status: STATUS_MAP[decoded[1] as number] ?? 'OK',
      model: MODEL_MAP[decoded[2] as number] ?? 'G3',
      tokens: decoded[3] as number,
      result: decoded[4] as string,
    };
  } else if (version === 3) {
    // v3: [3, 0, 0, "result"] or [3, 0, 0, 150, "result"]
    if (decoded.length === 4) {
      // tokens=0, omitted
      return {
        status: STATUS_MAP[decoded[1] as number] ?? 'OK',
        model: MODEL_MAP[decoded[2] as number] ?? 'G3',
        tokens: 0,
        result: decoded[3] as string,
      };
    } else {
      return {
        status: STATUS_MAP[decoded[1] as number] ?? 'OK',
        model: MODEL_MAP[decoded[2] as number] ?? 'G3',
        tokens: decoded[3] as number,
        result: decoded[4] as string,
      };
    }
  }

  throw new Error(`Unknown MessagePack version: ${version}`);
}

// ============================================================================
// Main Decoder
// ============================================================================

/**
 * Decode a Compact Protocol response
 * @param input Raw response string from LLM-MCP
 * @returns CompactResponse object
 */
export function decode(input: string): CompactResponse {
  const trimmed = input.trim();

  // Compressed (Zlib + Base85)
  if (trimmed.startsWith('Z')) {
    const base85 = trimmed.slice(1);
    const compressed = decodeBase85(base85);
    const decompressed = pako.inflate(compressed);
    return decodeMsgpack(decompressed);
  }

  // Base85 MessagePack
  if (trimmed.startsWith('A')) {
    const base85 = trimmed.slice(1);
    const data = decodeBase85(base85);
    return decodeMsgpack(data);
  }

  // Base64 MessagePack
  if (trimmed.startsWith('M')) {
    const base64 = trimmed.slice(1);
    const binary = Uint8Array.from(atob(base64), c => c.charCodeAt(0));
    return decodeMsgpack(binary);
  }

  // Legacy prefixes (v1.2 compatibility)
  if (trimmed.startsWith('A85:')) {
    const base85 = trimmed.slice(4);
    const data = decodeBase85(base85);
    return decodeMsgpack(data);
  }

  if (trimmed.startsWith('MPK:')) {
    const base64 = trimmed.slice(4);
    const binary = Uint8Array.from(atob(base64), c => c.charCodeAt(0));
    return decodeMsgpack(binary);
  }

  // Compact DSL: RES|OK|X5|0|result
  if (trimmed.startsWith('RES|')) {
    const parts = trimmed.split('|');
    if (parts.length < 5) {
      throw new Error('Invalid Compact DSL format');
    }

    return {
      status: parts[1] as Status,
      model: parts[2] as ModelCode,
      tokens: parseInt(parts[3], 10),
      // Rejoin remaining parts (result may contain |)
      result: parts.slice(4).join('|'),
    };
  }

  // Try JSON (Verbose format)
  try {
    const json = JSON.parse(trimmed) as VerboseResponse;
    return {
      status: json.returncode === 0 ? 'OK' : 'ERR',
      model: extractModelCode(json.model),
      tokens: 0, // Not in verbose format
      result: json.response,
    };
  } catch {
    throw new Error(`Unknown response format: ${trimmed.slice(0, 50)}...`);
  }
}

/**
 * Extract model code from full model name
 */
function extractModelCode(model: string): ModelCode {
  if (model.includes('gemini')) return 'G3';
  if (model.includes('claude')) return 'C4';
  if (model.includes('codex') || model.includes('gpt')) return 'X5';
  if (model.includes('ollama')) return 'OL';
  if (model.includes('adam')) return 'AD';
  if (model.includes('seele')) return 'SE';
  return model.slice(0, 2).toUpperCase() as ModelCode;
}

// ============================================================================
// Delta Streaming Decoder
// ============================================================================

export interface DeltaState {
  content: string;
}

export function createDeltaState(): DeltaState {
  return { content: '' };
}

/**
 * Apply a delta event to the state
 * @param state Current delta state
 * @param delta Delta string (D|F|..., D|+|..., D|R|pos|...)
 * @returns Updated content
 */
export function applyDelta(state: DeltaState, delta: string): string {
  // Compressed delta (D|Z+|..., D|ZF|..., D|ZR|...)
  if (delta.startsWith('D|Z')) {
    const op = delta[3]; // + or F or R
    const rest = delta.slice(5); // Skip D|Zx|
    const compressed = decodeBase85(rest);
    const decompressed = pako.inflate(compressed, { to: 'string' });

    if (op === 'F') {
      state.content = decompressed;
    } else if (op === '+') {
      state.content += decompressed;
    } else if (op === 'R') {
      // Decompressed format: pos|content
      const pipeIdx = decompressed.indexOf('|');
      const pos = parseInt(decompressed.slice(0, pipeIdx), 10);
      const newContent = decompressed.slice(pipeIdx + 1);
      state.content = state.content.slice(0, pos) + newContent;
    }
    return state.content;
  }

  // Full replace
  if (delta.startsWith('D|F|')) {
    state.content = delta.slice(4);
    return state.content;
  }

  // Append
  if (delta.startsWith('D|+|')) {
    state.content += delta.slice(4);
    return state.content;
  }

  // Replace at position
  if (delta.startsWith('D|R|')) {
    const rest = delta.slice(4);
    const pipeIdx = rest.indexOf('|');
    const pos = parseInt(rest.slice(0, pipeIdx), 10);
    const newContent = rest.slice(pipeIdx + 1);
    state.content = state.content.slice(0, pos) + newContent;
    return state.content;
  }

  throw new Error(`Unknown delta format: ${delta}`);
}

// ============================================================================
// Exports
// ============================================================================

export default {
  decode,
  createDeltaState,
  applyDelta,
  decodeBase85,
};
