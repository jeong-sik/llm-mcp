#!/usr/bin/env python3
"""
Cross-Format Small Message Test

MASC 메시지로 훈련한 dictionary가
다른 형식의 소형 메시지에도 효과 있는지 검증.

이게 통과해야 진짜 "워킹한다"고 할 수 있음.
"""

import zstandard as zstd
import json
from pathlib import Path
import statistics

# ============================================================
# 다양한 형식의 소형 메시지
# ============================================================

# Format A: MASC-style (훈련 데이터)
MASC_MESSAGES = [
    b'{"seq": 1, "from": "claude", "content": "Starting task", "timestamp": "2026-01-12T10:00:00Z"}',
    b'{"seq": 2, "from": "gemini", "content": "Task claimed", "timestamp": "2026-01-12T10:01:00Z"}',
    b'{"seq": 3, "from": "codex", "content": "File locked", "timestamp": "2026-01-12T10:02:00Z"}',
]

# Format B: HTTP API Response (다른 구조)
HTTP_MESSAGES = [
    b'{"status": 200, "data": {"id": "usr_123", "name": "John"}, "meta": {"version": "1.0"}}',
    b'{"status": 201, "data": {"id": "ord_456", "total": 99.99}, "meta": {"created": true}}',
    b'{"status": 404, "error": {"code": "NOT_FOUND", "message": "Resource not found"}}',
]

# Format C: Log entries (또 다른 구조)
LOG_MESSAGES = [
    b'[2026-01-12 10:00:00] INFO  main.py:42 - Server started on port 8080',
    b'[2026-01-12 10:00:01] DEBUG api.py:128 - Request received: GET /users',
    b'[2026-01-12 10:00:02] ERROR db.py:55 - Connection timeout after 30s',
]

# Format D: Slack-style (이모지 포함)
SLACK_MESSAGES = [
    b'{"type": "message", "user": "U123", "text": ":wave: Hello team!", "ts": "1704963600"}',
    b'{"type": "reaction", "user": "U456", "reaction": "thumbsup", "item_ts": "1704963601"}',
    b'{"type": "message", "user": "U789", "text": "LGTM :+1:", "thread_ts": "1704963600"}',
]

# Format E: MCP Tool Call (우리 실제 사용 케이스)
MCP_MESSAGES = [
    b'{"jsonrpc": "2.0", "method": "tools/call", "params": {"name": "read_file"}, "id": 1}',
    b'{"jsonrpc": "2.0", "result": {"content": "file data here"}, "id": 1}',
    b'{"jsonrpc": "2.0", "error": {"code": -32600, "message": "Invalid Request"}, "id": 2}',
]

def test_compression(messages, dict_bytes=None):
    """압축 테스트"""
    if dict_bytes:
        zdict = zstd.ZstdCompressionDict(dict_bytes)
        cctx = zstd.ZstdCompressor(level=3, dict_data=zdict)
    else:
        cctx = zstd.ZstdCompressor(level=3)

    total_orig = 0
    total_comp = 0

    for msg in messages:
        orig = len(msg)
        comp = len(cctx.compress(msg))
        total_orig += orig
        total_comp += comp

    ratio = (1 - total_comp / total_orig) * 100
    avg_size = total_orig / len(messages)

    return ratio, avg_size

def main():
    print("=" * 70)
    print("CROSS-FORMAT SMALL MESSAGE TEST")
    print("MASC dictionary가 다른 형식에도 효과 있나?")
    print("=" * 70)

    # 1. MASC 메시지로 dictionary 훈련
    print("\n[1] Training dictionary on MASC messages...")
    training_data = MASC_MESSAGES * 50  # 충분한 샘플
    dict_bytes = zstd.train_dictionary(16 * 1024, training_data).as_bytes()
    print(f"    Dictionary size: {len(dict_bytes):,} bytes")

    # 2. 각 형식별 테스트
    test_formats = {
        "MASC (same)": MASC_MESSAGES,
        "HTTP API": HTTP_MESSAGES,
        "Log entries": LOG_MESSAGES,
        "Slack-style": SLACK_MESSAGES,
        "MCP Tool": MCP_MESSAGES,
    }

    print("\n" + "=" * 70)
    print("RESULTS: MASC Dictionary → Various Formats")
    print("=" * 70)
    print(f"\n{'Format':<15} | {'Avg Size':>8} | {'No Dict':>8} | {'MASC Dict':>9} | {'Δ':>8}")
    print("-" * 60)

    results = {}
    for name, messages in test_formats.items():
        std_ratio, avg_size = test_compression(messages)
        dict_ratio, _ = test_compression(messages, dict_bytes)
        delta = dict_ratio - std_ratio

        print(f"{name:<15} | {avg_size:>6.0f} B | {std_ratio:>7.1f}% | {dict_ratio:>8.1f}% | {delta:>+7.1f}%p")
        results[name] = {"std": std_ratio, "dict": dict_ratio, "delta": delta}

    # 3. 분석
    print("\n" + "=" * 70)
    print("ANALYSIS")
    print("=" * 70)

    same_format_delta = results["MASC (same)"]["delta"]
    cross_format_deltas = [v["delta"] for k, v in results.items() if k != "MASC (same)"]
    avg_cross_delta = statistics.mean(cross_format_deltas)

    print(f"""
  Same-format (MASC→MASC):     {same_format_delta:+.1f}%p
  Cross-format average:        {avg_cross_delta:+.1f}%p

  Degradation:                 {same_format_delta - avg_cross_delta:.1f}%p
""")

    # 4. 판정
    print("=" * 70)
    print("VERDICT")
    print("=" * 70)

    if avg_cross_delta > 30:
        print(f"""
✅ Cross-format transfer 효과적!
   다른 형식에서도 평균 +{avg_cross_delta:.1f}%p 향상

→ "Universal small message dictionary" 가능성 있음
→ 워킹한다! 🎉
""")
    elif avg_cross_delta > 10:
        print(f"""
⚠️  Cross-format transfer 부분적 효과
   다른 형식에서 평균 +{avg_cross_delta:.1f}%p 향상

→ 일부 형식에서만 효과 있음
→ Format-specific dictionary 필요할 수 있음
""")
    else:
        print(f"""
❌ Cross-format transfer 실패
   다른 형식에서 평균 {avg_cross_delta:+.1f}%p

→ MASC dictionary는 MASC에만 효과
→ "Similar things are similar" 다시 등장
→ 각 형식별 dictionary 필요
""")

if __name__ == "__main__":
    main()
