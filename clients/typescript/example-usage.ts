/**
 * Compact Protocol v1.3 - TypeScript Client Usage Example
 *
 * 실제 llm-mcp 서버와 통신하는 예시입니다.
 * 서버 시작: cd ~/me/features/llm-mcp && dune exec llm-mcp -- --port 8932
 *
 * 실행: npx tsx example-usage.ts
 */

import {
  decode,
  decodeBase85,
  createDeltaState,
  applyDelta,
  CompactResponse,
} from './compact-decoder';

// === Configuration ===
const LLM_MCP_URL = 'http://localhost:8932/mcp';

interface MCPResponse {
  jsonrpc: string;
  id: number;
  result?: {
    content: Array<{ type: string; text: string }>;
  };
  error?: {
    code: number;
    message: string;
  };
}

/**
 * llm-mcp를 통해 LLM을 호출합니다.
 */
async function callLLM(
  tool: string,
  prompt: string,
  responseFormat: string = 'compact',
  options: Record<string, unknown> = {}
): Promise<CompactResponse> {
  const payload = {
    jsonrpc: '2.0',
    id: 1,
    method: 'tools/call',
    params: {
      name: tool,
      arguments: {
        prompt,
        response_format: responseFormat,
        ...options,
      },
    },
  };

  const response = await fetch(LLM_MCP_URL, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload),
  });

  if (!response.ok) {
    throw new Error(`HTTP ${response.status}: ${response.statusText}`);
  }

  const result = (await response.json()) as MCPResponse;

  if (result.error) {
    throw new Error(`MCP Error: ${result.error.message}`);
  }

  if (result.result?.content?.[0]?.text) {
    const rawText = result.result.content[0].text;
    return decode(rawText) as CompactResponse;
  }

  throw new Error(`Unexpected response: ${JSON.stringify(result)}`);
}

// === Examples ===

function exampleBase85Decode(): void {
  console.log('=== Base85 Decoding ===');

  // OCaml encode_base85("hello") = "Xk#0@Zv"
  const encoded = 'Xk#0@Zv';
  const decoded = decodeBase85(encoded);

  console.log(`Encoded: ${encoded}`);
  console.log(`Decoded: ${Buffer.from(decoded).toString()}`); // 'hello'
  console.log();
}

function exampleStreamingDelta(): void {
  console.log('=== Streaming Delta Protocol ===');

  const state = createDeltaState();

  // 실제로는 SSE나 WebSocket으로 받음
  const deltas = [
    'D|F|The answer',
    'D|+| to life',
    'D|+|, the universe',
    'D|+|, and everything',
    'D|+| is 42.',
  ];

  for (const delta of deltas) {
    const content = applyDelta(state, delta);
    console.log(`  → ${content}`);
  }

  console.log(`\nFinal: ${state.content}`);
  console.log();
}

async function exampleBasicCall(): Promise<void> {
  console.log('=== Basic Call (Compact DSL) ===');

  const result = await callLLM(
    'gemini',
    'What is 2+2? Reply with just the number.',
    'compact'
  );

  console.log(`Model: ${result.model}`);
  console.log(`Status: ${result.status}`);
  console.log(`Tokens: ${result.tokens}`);
  console.log(`Result: ${result.result}`);
  console.log();
}

async function exampleDifferentFormats(): Promise<void> {
  console.log('=== Different Response Formats ===');

  const prompt = 'Say hello';

  // Compact DSL
  console.log('Compact DSL:');
  const compact = await callLLM('gemini', prompt, 'compact');
  console.log(`  Result: ${compact.result}`);

  // Verbose JSON
  console.log('Verbose JSON:');
  const verbose = await callLLM('gemini', prompt, 'verbose');
  console.log(`  Result: ${verbose.result}`);

  console.log();
}

async function exampleMAGI(): Promise<void> {
  console.log('=== MAGI Pentarchy ===');

  const prompt = 'Define recursion in one sentence.';

  const llms: Array<[string, string]> = [
    ['gemini', 'CASPER (전략가)'],
    ['codex', 'MELCHIOR (과학자)'],
    // ['claude-cli', 'BALTHASAR'],  // 자기 자신은 skip
  ];

  for (const [tool, name] of llms) {
    try {
      const result = await callLLM(tool, prompt, 'compact');
      const truncated =
        result.result.length > 80
          ? result.result.slice(0, 80) + '...'
          : result.result;
      console.log(`${name}: ${truncated}`);
    } catch (e) {
      console.log(`${name}: Error - ${e}`);
    }
  }
  console.log();
}

async function checkServer(): Promise<boolean> {
  try {
    const response = await fetch('http://localhost:8932/health', {
      signal: AbortSignal.timeout(5000),
    });
    if (response.ok) {
      console.log('✅ llm-mcp server is running');
      return true;
    }
  } catch {
    // Server not running
  }

  console.log('❌ llm-mcp server is not running');
  console.log(
    '   Start with: cd ~/me/features/llm-mcp && dune exec llm-mcp -- --port 8932'
  );
  return false;
}

// === Main ===

async function main(): Promise<void> {
  console.log('Compact Protocol v1.3 - TypeScript Client Examples\n');

  // 서버 없이 실행 가능한 예시
  exampleBase85Decode();
  exampleStreamingDelta();

  // 서버 필요한 예시
  const serverRunning = await checkServer();
  if (serverRunning) {
    console.log();
    await exampleBasicCall();
    // await exampleDifferentFormats();
    // await exampleMAGI();
  }
}

main().catch(console.error);
