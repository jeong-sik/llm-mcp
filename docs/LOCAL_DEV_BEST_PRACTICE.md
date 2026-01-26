# Local MCP 테스트 → 실로컬 적용 (베스트 케이스)

목표: 변경사항을 **샌드박스 포트**에서 검증한 뒤, **실사용 포트(8932)** 로 승격한다.

## 0. 원칙

- Worktree first: main/develop 직접 수정 금지.
- 테스트/실사용 포트 분리 (예: 8939 → 8932).
- 형식 검증 우선: `chain.validate`(strict) 통과 후 실행.
- 실행 검증은 `stub` 모델로 최소 체인 스모크 테스트.

## 1. 워크트리 준비

```bash
git worktree add ../.worktrees/llm-mcp-local-<tag> -b codex/<tag>
cd ../.worktrees/llm-mcp-local-<tag>
```

## 2. 로컬 빌드 (선택)

```bash
opam install . --deps-only
dune build ./bin/main_eio.exe
```

## 3. 샌드박스 서버 실행 (테스트 포트)

```bash
./start-llm-mcp.sh --port 8939
```

### 3-1. 헬스 체크 + 툴 목록

```bash
curl http://127.0.0.1:8939/health

curl -sS http://127.0.0.1:8939/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'
```

### 3-2. 체인 형식 검증 (strict)

```bash
curl -sS http://127.0.0.1:8939/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc":"2.0",
    "id":2,
    "method":"tools/call",
    "params":{
      "name":"chain.validate",
      "arguments":{
        "strict":true,
        "chain":{
          "id":"smoke_validate",
          "nodes":[
            {"id":"a","type":"llm","model":"stub","prompt":"ping"}
          ],
          "output":"a"
        }
      }
    }
  }'
```

### 3-3. 체인 실행 스모크 테스트 (stub)

```bash
curl -sS http://127.0.0.1:8939/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc":"2.0",
    "id":3,
    "method":"tools/call",
    "params":{
      "name":"chain.run",
      "arguments":{
        "chain":{
          "id":"smoke_run",
          "nodes":[
            {"id":"a","type":"llm","model":"stub","prompt":"ping {{input}}"}
          ],
          "output":"a"
        },
        "input":"OK",
        "trace":true,
        "timeout":10
      }
    }
  }'
```

## 4. 실로컬 승격 (포트 8932)

샌드박스 테스트가 통과하면 실사용 포트로 재기동:

```bash
./start-llm-mcp.sh --port 8932
```

`~/.mcp.json`은 8932 포인트로 유지:

```json
{
  "mcpServers": {
    "llm-mcp": {
      "type": "http",
      "url": "http://127.0.0.1:8932/mcp"
    }
  }
}
```

## 5. 운영 체크리스트 (요약)

- `chain.validate` strict 통과
- `stub` 스모크 테스트 통과
- 실사용 포트(8932)에서 `/health` 정상
- 필요 시 `data/chain_history.jsonl`에서 실패 이벤트 확인

