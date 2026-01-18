#!/usr/bin/env python3
"""
Small Message Compression Test

실제 MASC 메시지 (100-500 bytes) 압축 테스트.
Dictionary가 소형 메시지에서 효과 있는지 검증.
"""

import zstandard as zstd
import json
from pathlib import Path
from typing import List, Tuple
import statistics

def load_masc_messages(masc_dir: Path) -> List[bytes]:
    """실제 MASC 메시지 로드"""
    messages = []
    msg_dir = masc_dir / "messages"

    if msg_dir.exists():
        for f in sorted(msg_dir.glob("*.json"))[:100]:
            try:
                messages.append(f.read_bytes())
            except:
                pass

    return messages

def create_synthetic_messages(count: int = 100) -> List[bytes]:
    """MASC 스타일 합성 메시지 생성"""
    import random

    agents = ["claude", "gemini", "codex", "ollama", "system"]
    actions = [
        "👋 {agent} joined the room",
        "📋 Claimed task: {task}",
        "✅ Completed: {task}",
        "🔒 Locked file: {file}",
        "🔓 Unlocked file: {file}",
        "💬 @{target}: {msg}",
        "🚀 Starting work on {task}",
        "⚠️ Error in {file}: {error}",
    ]
    tasks = ["PK-12345", "fix-auth", "update-deps", "refactor-api", "add-tests"]
    files = ["src/main.ts", "lib/utils.py", "api/handler.go", "config.json"]

    messages = []
    for i in range(count):
        agent = random.choice(agents)
        action = random.choice(actions)
        content = action.format(
            agent=agent,
            task=random.choice(tasks),
            file=random.choice(files),
            target=random.choice(agents),
            msg="can you review this?",
            error="type mismatch"
        )

        msg = {
            "seq": i + 1,
            "from": agent,
            "content": content,
            "timestamp": f"2026-01-12T{10 + i//60:02d}:{i%60:02d}:00Z"
        }
        messages.append(json.dumps(msg, indent=2).encode())

    return messages

def test_compression(messages: List[bytes], dict_bytes: bytes = None) -> dict:
    """압축 테스트"""
    results = {
        "count": len(messages),
        "total_original": 0,
        "total_compressed": 0,
        "ratios": [],
        "sizes": [],
        "negative_count": 0,  # 압축 후 더 커진 경우
    }

    if dict_bytes:
        zdict = zstd.ZstdCompressionDict(dict_bytes)
        cctx = zstd.ZstdCompressor(level=3, dict_data=zdict)
    else:
        cctx = zstd.ZstdCompressor(level=3)

    for msg in messages:
        original = len(msg)
        compressed = len(cctx.compress(msg))
        ratio = (1 - compressed / original) * 100

        results["total_original"] += original
        results["total_compressed"] += compressed
        results["ratios"].append(ratio)
        results["sizes"].append(original)

        if compressed >= original:
            results["negative_count"] += 1

    results["avg_ratio"] = statistics.mean(results["ratios"])
    results["overall_ratio"] = (1 - results["total_compressed"] / results["total_original"]) * 100
    results["avg_size"] = statistics.mean(results["sizes"])

    return results

def main():
    print("=" * 70)
    print("SMALL MESSAGE COMPRESSION TEST")
    print("실제 Agent↔Agent 메시지 흐름 압축 효과 측정")
    print("=" * 70)

    masc_dir = Path.home() / "me" / ".masc"

    # 1. 실제 MASC 메시지 로드
    real_messages = load_masc_messages(masc_dir)
    print(f"\n[1] Real MASC Messages: {len(real_messages)} loaded")

    # 2. 합성 메시지 생성 (더 많은 샘플)
    synthetic_messages = create_synthetic_messages(200)
    print(f"[2] Synthetic Messages: {len(synthetic_messages)} generated")

    # 메시지 합치기
    all_messages = real_messages + synthetic_messages
    print(f"[3] Total Messages: {len(all_messages)}")

    if len(all_messages) < 10:
        print("Not enough messages!")
        return

    # 3. Train/Test 분할
    train_messages = all_messages[:len(all_messages)//2]
    test_messages = all_messages[len(all_messages)//2:]

    print(f"\n    Train: {len(train_messages)} messages")
    print(f"    Test:  {len(test_messages)} messages")

    # 4. Dictionary 훈련
    print("\n[4] Training dictionary on small messages...")
    try:
        # 작은 메시지용 작은 dictionary
        dict_bytes = zstd.train_dictionary(16 * 1024, train_messages * 5).as_bytes()
        print(f"    Dictionary size: {len(dict_bytes):,} bytes")
    except Exception as e:
        print(f"    Training failed: {e}")
        dict_bytes = None

    # 5. 압축 테스트
    print("\n" + "=" * 70)
    print("COMPRESSION RESULTS")
    print("=" * 70)

    # No compression baseline
    no_comp = {"total_original": sum(len(m) for m in test_messages)}

    # Standard zstd (no dict)
    std_results = test_compression(test_messages, dict_bytes=None)

    # Dictionary zstd
    if dict_bytes:
        dict_results = test_compression(test_messages, dict_bytes=dict_bytes)
    else:
        dict_results = std_results

    print(f"""
┌─────────────────────────────────────────────────────────────────┐
│  SMALL MESSAGE COMPRESSION (<500 bytes)                         │
├─────────────────────────────────────────────────────────────────┤
│  Messages tested:      {len(test_messages):3d}                                       │
│  Avg message size:     {std_results['avg_size']:.0f} bytes                              │
├─────────────────────────────────────────────────────────────────┤
│  Standard zstd:        {std_results['overall_ratio']:5.1f}% compression                     │
│  Negative (got bigger):{std_results['negative_count']:3d} messages                          │
├─────────────────────────────────────────────────────────────────┤
│  Dictionary zstd:      {dict_results['overall_ratio']:5.1f}% compression                     │
│  Negative (got bigger):{dict_results['negative_count']:3d} messages                          │
├─────────────────────────────────────────────────────────────────┤
│  Dictionary Advantage: {dict_results['overall_ratio'] - std_results['overall_ratio']:+5.1f}%p                                │
└─────────────────────────────────────────────────────────────────┘
""")

    # 6. 크기별 분석
    print("\n[SIZE BREAKDOWN]")
    print("-" * 50)

    size_buckets = [(0, 100), (100, 200), (200, 500), (500, 1000)]

    for min_size, max_size in size_buckets:
        bucket_msgs = [m for m in test_messages if min_size <= len(m) < max_size]
        if not bucket_msgs:
            continue

        std_r = test_compression(bucket_msgs, dict_bytes=None)
        dict_r = test_compression(bucket_msgs, dict_bytes=dict_bytes) if dict_bytes else std_r

        advantage = dict_r['overall_ratio'] - std_r['overall_ratio']

        print(f"  {min_size:3d}-{max_size:3d} bytes ({len(bucket_msgs):3d} msgs):")
        print(f"    Standard: {std_r['overall_ratio']:5.1f}%  |  Dict: {dict_r['overall_ratio']:5.1f}%  |  Δ: {advantage:+5.1f}%p")

    # 7. 결론
    advantage = dict_results['overall_ratio'] - std_results['overall_ratio']

    print("\n" + "=" * 70)
    print("VERDICT")
    print("=" * 70)

    if advantage > 10:
        print(f"""
✅ Dictionary가 소형 메시지에서 효과적!
   +{advantage:.1f}%p 향상

→ MASC 메시지 압축에 Dictionary 적용 추천
""")
    elif advantage > 0:
        print(f"""
⚠️  Dictionary 효과 미미 ({advantage:+.1f}%p)

→ 구현 복잡성 대비 이점 불명확
→ 표준 zstd로 충분할 수 있음
""")
    else:
        print(f"""
❌ Dictionary 효과 없음 ({advantage:+.1f}%p)

→ 소형 메시지에서도 Dictionary 불필요
→ 표준 zstd 또는 압축 안함이 나을 수 있음
""")

    # 압축 자체가 의미없는 경우
    if std_results['negative_count'] > len(test_messages) * 0.3:
        print(f"""
⚠️  주의: {std_results['negative_count']}/{len(test_messages)} 메시지가 압축 후 더 커짐
   → 100-200 bytes 메시지는 압축 안 하는 게 나을 수 있음
""")

if __name__ == "__main__":
    main()
