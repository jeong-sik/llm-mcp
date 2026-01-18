#!/usr/bin/env python3
"""
Adversarial Robustness Test for Compact Protocol v4

Reviewer question: "Can carefully crafted prompts force degenerate compression?"

Test cases:
1. High-entropy random data (incompressible)
2. Repeated patterns that break dictionary
3. Unusual Unicode/binary-like sequences
4. Anti-patterns that avoid dictionary matches
5. Maximally diverse character sets
"""

import zstandard as zstd
import lz4.frame
import brotli
import gzip
import json
import time
import random
import string
from pathlib import Path
from dataclasses import dataclass
from typing import List

# ============================================================
# Adversarial Payloads
# ============================================================

def generate_random_bytes(size: int) -> bytes:
    """Pure random bytes - theoretical worst case for any compressor."""
    return bytes(random.getrandbits(8) for _ in range(size))


def generate_random_ascii(size: int) -> bytes:
    """Random ASCII - high entropy text."""
    chars = string.ascii_letters + string.digits + string.punctuation
    return ''.join(random.choice(chars) for _ in range(size)).encode()


def generate_anti_pattern(size: int) -> bytes:
    """Text designed to avoid common LLM patterns."""
    # Avoid: def, class, function, import, return, {, }, #, *, -
    chars = "abceghjklmnoqvwxyz0123456789ABCEGHJKLMNOQVWXYZ "
    return ''.join(random.choice(chars) for _ in range(size)).encode()


def generate_unique_words(count: int) -> bytes:
    """Many unique words - defeats dictionary."""
    words = [''.join(random.choices(string.ascii_lowercase, k=random.randint(3, 12)))
             for _ in range(count)]
    return ' '.join(words).encode()


def generate_unicode_chaos(size: int) -> bytes:
    """Mixed Unicode from many scripts."""
    scripts = [
        "가나다라마바사아자차카타파하",  # Korean
        "你好世界中文测试数据",  # Chinese
        "αβγδεζηθικλμνξοπρστυφχψω",  # Greek
        "абвгдеёжзийклмнопрстуфхцчшщъыьэюя",  # Cyrillic
        "あいうえおかきくけこさしすせそ",  # Japanese Hiragana
        "🎉🔥💡🚀✨🎯🔧💻🎨🌟",  # Emoji
        "←→↑↓↔↕↖↗↘↙",  # Arrows
    ]
    result = []
    while len(''.join(result).encode()) < size:
        result.append(random.choice(random.choice(scripts)))
    return ''.join(result).encode()[:size]


def generate_binary_like(size: int) -> bytes:
    """Text that looks like binary/hex dumps."""
    hex_chars = "0123456789abcdef"
    lines = []
    for i in range(size // 50):
        addr = f"{i*16:08x}"
        data = ' '.join(''.join(random.choices(hex_chars, k=2)) for _ in range(16))
        lines.append(f"{addr}: {data}")
    return '\n'.join(lines).encode()[:size]


def generate_json_chaos(count: int) -> bytes:
    """Deeply nested, random JSON."""
    def random_value(depth=0):
        if depth > 5:
            return random.choice([random.randint(0, 1000),
                                  ''.join(random.choices(string.ascii_letters, k=8)),
                                  random.random()])
        choice = random.randint(0, 4)
        if choice == 0:
            return {f"k{i}": random_value(depth+1) for i in range(random.randint(1, 5))}
        elif choice == 1:
            return [random_value(depth+1) for _ in range(random.randint(1, 5))]
        else:
            return random.choice([random.randint(0, 1000),
                                  ''.join(random.choices(string.ascii_letters, k=8))])

    obj = {f"root{i}": random_value() for i in range(count)}
    return json.dumps(obj).encode()


ADVERSARIAL_CASES = [
    ("random_bytes_1k", lambda: generate_random_bytes(1024)),
    ("random_ascii_1k", lambda: generate_random_ascii(1024)),
    ("anti_pattern_1k", lambda: generate_anti_pattern(1024)),
    ("unique_words_200", lambda: generate_unique_words(200)),
    ("unicode_chaos_1k", lambda: generate_unicode_chaos(1024)),
    ("binary_like_1k", lambda: generate_binary_like(1024)),
    ("json_chaos_deep", lambda: generate_json_chaos(10)),
    ("random_bytes_5k", lambda: generate_random_bytes(5000)),
    ("mixed_adversarial", lambda: (
        generate_random_ascii(200) +
        generate_unicode_chaos(200) +
        generate_binary_like(200) +
        generate_unique_words(50)
    )),
]

# Normal LLM-like data for comparison
NORMAL_CASES = [
    ("python_code", lambda: b'''
def merge_sort(arr):
    if len(arr) <= 1:
        return arr
    mid = len(arr) // 2
    left = merge_sort(arr[:mid])
    right = merge_sort(arr[mid:])
    return merge(left, right)

def merge(left, right):
    result = []
    i = j = 0
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1
    result.extend(left[i:])
    result.extend(right[j:])
    return result
'''),
    ("json_api", lambda: b'''
{
  "status": "success",
  "data": {
    "user": {
      "id": 12345,
      "name": "John Doe",
      "email": "john@example.com",
      "roles": ["admin", "user"],
      "preferences": {
        "theme": "dark",
        "notifications": true
      }
    }
  },
  "metadata": {
    "timestamp": "2026-01-12T12:00:00Z",
    "version": "1.0"
  }
}
'''),
    ("markdown_doc", lambda: b'''
# API Documentation

## Overview
This API provides access to user data and preferences.

### Endpoints

| Method | Path | Description |
|--------|------|-------------|
| GET | /users | List all users |
| POST | /users | Create user |
| GET | /users/:id | Get user by ID |

## Authentication
All requests require Bearer token authentication.

```python
headers = {"Authorization": "Bearer <token>"}
```
'''),
]


# ============================================================
# Benchmark
# ============================================================

@dataclass
class Result:
    name: str
    category: str
    size: int
    compact_ratio: float
    zstd_ratio: float
    gzip_ratio: float
    verdict: str  # "robust" or "degraded"


def compress_all(data: bytes, zstd_dict: bytes) -> dict:
    """Compress with all methods."""
    results = {}

    # Gzip
    compressed = gzip.compress(data)
    results['gzip'] = (1 - len(compressed) / len(data)) * 100

    # Zstd (default)
    cctx = zstd.ZstdCompressor(level=3)
    compressed = cctx.compress(data)
    results['zstd'] = (1 - len(compressed) / len(data)) * 100

    # Compact v4 (zstd-dict)
    zd = zstd.ZstdCompressionDict(zstd_dict)
    cctx_dict = zstd.ZstdCompressor(level=3, dict_data=zd)
    compressed = cctx_dict.compress(data)
    results['compact'] = (1 - len(compressed) / len(data)) * 100

    return results


def run_adversarial_test():
    print("=" * 70)
    print("Adversarial Robustness Test - Compact Protocol v4")
    print("=" * 70)
    print()
    print("Question: Can malicious inputs break compression?")
    print()

    # Train dictionary on normal LLM outputs
    print("[1/3] Training dictionary on NORMAL LLM outputs...")
    training_data = [gen() for _, gen in NORMAL_CASES] * 5
    zstd_dict = zstd.train_dictionary(110_000, training_data).as_bytes()
    print(f"      Dictionary size: {len(zstd_dict):,} bytes")
    print()

    results = []

    # Test normal cases first
    print("[2/3] Testing NORMAL inputs (baseline)...")
    print("-" * 70)
    for name, gen in NORMAL_CASES:
        data = gen()
        ratios = compress_all(data, zstd_dict)
        result = Result(
            name=name,
            category="normal",
            size=len(data),
            compact_ratio=ratios['compact'],
            zstd_ratio=ratios['zstd'],
            gzip_ratio=ratios['gzip'],
            verdict="baseline"
        )
        results.append(result)
        print(f"  ✓ {name:20s} | {len(data):5,}B | "
              f"Compact:{ratios['compact']:5.1f}% | "
              f"Zstd:{ratios['zstd']:5.1f}% | "
              f"Gzip:{ratios['gzip']:5.1f}%")

    # Test adversarial cases
    print()
    print("[3/3] Testing ADVERSARIAL inputs...")
    print("-" * 70)

    for name, gen in ADVERSARIAL_CASES:
        data = gen()
        ratios = compress_all(data, zstd_dict)

        # Verdict: degraded if Compact < Zstd (dictionary hurts)
        if ratios['compact'] < ratios['zstd'] - 5:
            verdict = "⚠️ degraded"
        elif ratios['compact'] < 20:
            verdict = "⚠️ low"
        else:
            verdict = "✅ robust"

        result = Result(
            name=name,
            category="adversarial",
            size=len(data),
            compact_ratio=ratios['compact'],
            zstd_ratio=ratios['zstd'],
            gzip_ratio=ratios['gzip'],
            verdict=verdict
        )
        results.append(result)

        print(f"  {verdict:12s} {name:20s} | {len(data):5,}B | "
              f"Compact:{ratios['compact']:5.1f}% | "
              f"Zstd:{ratios['zstd']:5.1f}% | "
              f"Gzip:{ratios['gzip']:5.1f}%")

    # Summary
    print()
    print("=" * 70)
    print("SUMMARY")
    print("=" * 70)

    normal_results = [r for r in results if r.category == "normal"]
    adv_results = [r for r in results if r.category == "adversarial"]

    normal_avg = sum(r.compact_ratio for r in normal_results) / len(normal_results)
    adv_avg = sum(r.compact_ratio for r in adv_results) / len(adv_results)

    degraded = [r for r in adv_results if "degraded" in r.verdict or "low" in r.verdict]

    print(f"\n  Normal inputs avg:      {normal_avg:.1f}% compression")
    print(f"  Adversarial inputs avg: {adv_avg:.1f}% compression")
    print(f"  Degradation:            {normal_avg - adv_avg:.1f}%")
    print(f"\n  Degraded cases: {len(degraded)}/{len(adv_results)}")

    if degraded:
        print("\n  Worst cases:")
        for r in degraded:
            print(f"    - {r.name}: {r.compact_ratio:.1f}% (vs {r.zstd_ratio:.1f}% zstd)")

    # Conclusion
    print("\n┌─────────────────────────────────────────────────────────────────┐")
    if adv_avg > 20 and len(degraded) <= 2:
        print("│  VERDICT: ✅ ROBUST                                             │")
        print("│  Adversarial inputs reduce compression but don't break it.     │")
    elif adv_avg > 0:
        print("│  VERDICT: ⚠️ PARTIALLY ROBUST                                   │")
        print("│  Some adversarial inputs significantly degrade performance.    │")
    else:
        print("│  VERDICT: ❌ VULNERABLE                                         │")
        print("│  Adversarial inputs can break compression.                     │")
    print("├─────────────────────────────────────────────────────────────────┤")
    print("│  NOTE: Random/high-entropy data is incompressible by ANY       │")
    print("│  algorithm. This is a fundamental information theory limit.    │")
    print("└─────────────────────────────────────────────────────────────────┘")

    # Save results
    output = {
        'date': time.strftime('%Y-%m-%d'),
        'normal_avg_compression': round(normal_avg, 2),
        'adversarial_avg_compression': round(adv_avg, 2),
        'degradation_percent': round(normal_avg - adv_avg, 2),
        'degraded_cases': len(degraded),
        'total_adversarial_cases': len(adv_results),
        'results': [
            {
                'name': r.name,
                'category': r.category,
                'size': r.size,
                'compact_ratio': round(r.compact_ratio, 2),
                'zstd_ratio': round(r.zstd_ratio, 2),
                'gzip_ratio': round(r.gzip_ratio, 2),
                'verdict': r.verdict,
            }
            for r in results
        ]
    }

    output_path = Path(__file__).parent.parent / 'data' / 'adversarial_results.json'
    with open(output_path, 'w') as f:
        json.dump(output, f, indent=2)
    print(f"\nResults saved to: {output_path}")


if __name__ == '__main__':
    run_adversarial_test()
