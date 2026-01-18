#!/usr/bin/env python3
"""
Multi-Format Dictionary Test

개선 시도:
1. 모든 형식 혼합 훈련
2. Dictionary 크기 증가
3. 결과 비교
"""

import zstandard as zstd
import statistics

# 다양한 형식의 소형 메시지
FORMATS = {
    "MASC": [
        b'{"seq": 1, "from": "claude", "content": "Starting task", "timestamp": "2026-01-12T10:00:00Z"}',
        b'{"seq": 2, "from": "gemini", "content": "Task claimed", "timestamp": "2026-01-12T10:01:00Z"}',
        b'{"seq": 3, "from": "codex", "content": "Completed work", "timestamp": "2026-01-12T10:02:00Z"}',
        b'{"seq": 4, "from": "system", "content": "Task done", "timestamp": "2026-01-12T10:03:00Z"}',
    ],
    "HTTP": [
        b'{"status": 200, "data": {"id": "usr_123", "name": "John"}, "meta": {"version": "1.0"}}',
        b'{"status": 201, "data": {"id": "ord_456", "total": 99.99}, "meta": {"created": true}}',
        b'{"status": 404, "error": {"code": "NOT_FOUND", "message": "Resource not found"}}',
        b'{"status": 500, "error": {"code": "SERVER_ERROR", "message": "Internal error"}}',
    ],
    "Slack": [
        b'{"type": "message", "user": "U123", "text": "Hello team!", "ts": "1704963600"}',
        b'{"type": "reaction", "user": "U456", "reaction": "thumbsup", "item_ts": "1704963601"}',
        b'{"type": "message", "user": "U789", "text": "LGTM", "thread_ts": "1704963600"}',
        b'{"type": "channel_join", "user": "U111", "channel": "C123", "ts": "1704963602"}',
    ],
    "MCP": [
        b'{"jsonrpc": "2.0", "method": "tools/call", "params": {"name": "read_file"}, "id": 1}',
        b'{"jsonrpc": "2.0", "result": {"content": "file data here"}, "id": 1}',
        b'{"jsonrpc": "2.0", "error": {"code": -32600, "message": "Invalid"}, "id": 2}',
        b'{"jsonrpc": "2.0", "method": "tools/list", "params": {}, "id": 3}',
    ],
    "Log": [
        b'[2026-01-12 10:00:00] INFO  main.py:42 - Server started on port 8080',
        b'[2026-01-12 10:00:01] DEBUG api.py:128 - Request received: GET /users',
        b'[2026-01-12 10:00:02] ERROR db.py:55 - Connection timeout after 30s',
        b'[2026-01-12 10:00:03] WARN  cache.py:99 - Cache miss for key: user_123',
    ],
}

def compress_ratio(messages, dict_bytes=None):
    if dict_bytes:
        zdict = zstd.ZstdCompressionDict(dict_bytes)
        cctx = zstd.ZstdCompressor(level=3, dict_data=zdict)
    else:
        cctx = zstd.ZstdCompressor(level=3)

    total_orig = sum(len(m) for m in messages)
    total_comp = sum(len(cctx.compress(m)) for m in messages)
    return (1 - total_comp / total_orig) * 100

def main():
    print("=" * 70)
    print("MULTI-FORMAT DICTIONARY EXPERIMENT")
    print("=" * 70)

    # 모든 메시지 합치기
    all_messages = []
    for msgs in FORMATS.values():
        all_messages.extend(msgs)

    print(f"\nTotal messages: {len(all_messages)}")
    print(f"Formats: {list(FORMATS.keys())}")

    # ============================================================
    # 실험 1: Format-specific vs Multi-format dictionary
    # ============================================================
    print("\n" + "=" * 70)
    print("EXPERIMENT 1: Single-format vs Multi-format Dictionary")
    print("=" * 70)

    # Multi-format dictionary 훈련
    multi_dict = zstd.train_dictionary(32 * 1024, all_messages * 30).as_bytes()
    print(f"\nMulti-format dict size: {len(multi_dict):,} bytes")

    print(f"\n{'Format':<10} | {'No Dict':>8} | {'Own Dict':>8} | {'Multi Dict':>10} | {'Multi Δ':>8}")
    print("-" * 60)

    multi_results = []
    for name, messages in FORMATS.items():
        # No dict
        no_dict = compress_ratio(messages)

        # Format-specific dict
        own_dict = zstd.train_dictionary(16 * 1024, messages * 30).as_bytes()
        own_ratio = compress_ratio(messages, own_dict)

        # Multi-format dict
        multi_ratio = compress_ratio(messages, multi_dict)
        multi_delta = multi_ratio - no_dict

        multi_results.append(multi_delta)
        print(f"{name:<10} | {no_dict:>7.1f}% | {own_ratio:>7.1f}% | {multi_ratio:>9.1f}% | {multi_delta:>+7.1f}%p")

    avg_multi = statistics.mean(multi_results)
    print(f"\n  Multi-format dict 평균 개선: {avg_multi:+.1f}%p")

    # ============================================================
    # 실험 2: Dictionary 크기 영향
    # ============================================================
    print("\n" + "=" * 70)
    print("EXPERIMENT 2: Dictionary Size Impact")
    print("=" * 70)

    dict_sizes = [8, 16, 32, 64, 128]  # KB

    print(f"\n{'Size':>8} | ", end="")
    for name in FORMATS.keys():
        print(f"{name:>8} | ", end="")
    print("Avg")
    print("-" * 70)

    for size_kb in dict_sizes:
        size_bytes = size_kb * 1024
        try:
            test_dict = zstd.train_dictionary(size_bytes, all_messages * 50).as_bytes()
        except:
            continue

        results = []
        print(f"{size_kb:>6} KB | ", end="")
        for name, messages in FORMATS.items():
            no_dict = compress_ratio(messages)
            with_dict = compress_ratio(messages, test_dict)
            delta = with_dict - no_dict
            results.append(delta)
            print(f"{delta:>+7.1f}% | ", end="")
        print(f"{statistics.mean(results):>+.1f}%")

    # ============================================================
    # 실험 3: JSON-only 공통 패턴
    # ============================================================
    print("\n" + "=" * 70)
    print("EXPERIMENT 3: JSON-only (excluding Log format)")
    print("=" * 70)

    json_formats = {k: v for k, v in FORMATS.items() if k != "Log"}
    json_messages = []
    for msgs in json_formats.values():
        json_messages.extend(msgs)

    json_dict = zstd.train_dictionary(32 * 1024, json_messages * 30).as_bytes()
    print(f"\nJSON-only dict size: {len(json_dict):,} bytes")

    print(f"\n{'Format':<10} | {'No Dict':>8} | {'JSON Dict':>9} | {'Δ':>8}")
    print("-" * 45)

    json_results = []
    for name, messages in json_formats.items():
        no_dict = compress_ratio(messages)
        json_ratio = compress_ratio(messages, json_dict)
        delta = json_ratio - no_dict
        json_results.append(delta)
        print(f"{name:<10} | {no_dict:>7.1f}% | {json_ratio:>8.1f}% | {delta:>+7.1f}%p")

    avg_json = statistics.mean(json_results)
    print(f"\n  JSON-only dict 평균 개선: {avg_json:+.1f}%p")

    # ============================================================
    # 결론
    # ============================================================
    print("\n" + "=" * 70)
    print("CONCLUSION")
    print("=" * 70)

    print(f"""
  Single-format dict:  각자 +60~80%p (최고)
  Multi-format dict:   평균 {avg_multi:+.1f}%p
  JSON-only dict:      평균 {avg_json:+.1f}%p
""")

    if avg_multi > 30:
        print("✅ Multi-format dictionary 효과적!")
        print("   → 하나의 dictionary로 여러 형식 압축 가능")
    elif avg_multi > 10:
        print("⚠️  Multi-format dictionary 부분적 효과")
        print("   → 일부 손실 감수하면 사용 가능")
    else:
        print("❌ Multi-format dictionary 효과 미미")
        print("   → Format-specific dictionary가 더 나음")

if __name__ == "__main__":
    main()
