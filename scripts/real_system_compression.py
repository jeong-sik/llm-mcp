#!/usr/bin/env python3
"""
Real System Compression Benchmark

실제 Second Brain 세션 데이터에 zstd 압축 적용.
이론이 아닌 실전 절약량 측정.
"""

import zstandard as zstd
import json
import os
from pathlib import Path
from dataclasses import dataclass
from typing import List, Tuple
import time

@dataclass
class CompressionResult:
    file_name: str
    original_size: int
    compressed_size: int
    ratio: float
    time_ms: float

def compress_file(path: Path, level: int = 3) -> CompressionResult:
    """파일 압축 테스트 (메모리에서)"""
    data = path.read_bytes()
    original_size = len(data)

    start = time.perf_counter()
    cctx = zstd.ZstdCompressor(level=level)
    compressed = cctx.compress(data)
    elapsed_ms = (time.perf_counter() - start) * 1000

    compressed_size = len(compressed)
    ratio = (1 - compressed_size / original_size) * 100

    return CompressionResult(
        file_name=path.name,
        original_size=original_size,
        compressed_size=compressed_size,
        ratio=ratio,
        time_ms=elapsed_ms
    )

def train_dictionary_from_sessions(session_dir: Path, dict_size: int = 110 * 1024) -> bytes:
    """세션 파일들로 dictionary 훈련"""
    samples = []

    # JSONL 파일에서 샘플 수집 (최대 100개 파일)
    jsonl_files = sorted(session_dir.glob("*.jsonl"), key=lambda p: p.stat().st_size, reverse=True)[:50]

    for jsonl_path in jsonl_files:
        try:
            content = jsonl_path.read_bytes()
            # 큰 파일은 청크로 분할
            chunk_size = 32 * 1024  # 32KB chunks
            for i in range(0, min(len(content), 256 * 1024), chunk_size):
                samples.append(content[i:i + chunk_size])
        except Exception as e:
            print(f"  Warning: {jsonl_path.name}: {e}")

    print(f"  Collected {len(samples)} samples from {len(jsonl_files)} files")

    if len(samples) < 10:
        print("  Not enough samples for dictionary training")
        return None

    # Dictionary 훈련
    try:
        zdict = zstd.train_dictionary(dict_size, samples)
        return zdict.as_bytes()
    except Exception as e:
        print(f"  Dictionary training failed: {e}")
        return None

def compress_with_dict(data: bytes, zdict_bytes: bytes, level: int = 3) -> Tuple[bytes, float]:
    """Dictionary로 압축"""
    zdict = zstd.ZstdCompressionDict(zdict_bytes)
    cctx = zstd.ZstdCompressor(level=level, dict_data=zdict)

    start = time.perf_counter()
    compressed = cctx.compress(data)
    elapsed_ms = (time.perf_counter() - start) * 1000

    return compressed, elapsed_ms

def format_size(size_bytes: int) -> str:
    """바이트를 읽기 쉬운 형식으로"""
    for unit in ['B', 'KB', 'MB', 'GB']:
        if size_bytes < 1024:
            return f"{size_bytes:.1f} {unit}"
        size_bytes /= 1024
    return f"{size_bytes:.1f} TB"

def main():
    print("=" * 70)
    print("REAL SYSTEM COMPRESSION BENCHMARK")
    print("Second Brain 세션 데이터 실측")
    print("=" * 70)

    # 1. 세션 디렉토리 찾기
    me_root = Path(os.getenv("ME_ROOT", str(Path.home() / "me")))
    project_slug = f"-{me_root.as_posix().lstrip('/')}".replace("/", "-")
    session_dir = Path.home() / ".claude" / "projects" / project_slug
    masc_dir = me_root / ".masc"

    results = {
        "sessions": [],
        "masc": [],
        "totals": {
            "original": 0,
            "zstd_standard": 0,
            "zstd_dict": 0,
        }
    }

    # 2. 세션 JSONL 파일 분석
    print(f"\n[1] SESSION FILES: {session_dir}")
    print("-" * 50)

    jsonl_files = sorted(session_dir.glob("*.jsonl"), key=lambda p: p.stat().st_size, reverse=True)

    if not jsonl_files:
        print("  No JSONL files found!")
        return

    # Dictionary 훈련 (상위 50개 파일 기반)
    print("\n  Training dictionary from session data...")
    session_dict = train_dictionary_from_sessions(session_dir)
    if session_dict:
        print(f"  Dictionary size: {format_size(len(session_dict))}")

    # 상위 20개 파일 테스트
    print(f"\n  Testing top 20 files (of {len(jsonl_files)} total)...")

    total_original = 0
    total_zstd = 0
    total_zstd_dict = 0

    for jsonl_path in jsonl_files[:20]:
        try:
            data = jsonl_path.read_bytes()
            original_size = len(data)
            total_original += original_size

            # Standard zstd
            cctx = zstd.ZstdCompressor(level=3)
            compressed_std = cctx.compress(data)
            std_size = len(compressed_std)
            total_zstd += std_size
            std_ratio = (1 - std_size / original_size) * 100

            # Dictionary zstd
            if session_dict:
                compressed_dict, _ = compress_with_dict(data, session_dict)
                dict_size = len(compressed_dict)
                total_zstd_dict += dict_size
                dict_ratio = (1 - dict_size / original_size) * 100
                dict_advantage = dict_ratio - std_ratio
            else:
                dict_ratio = 0
                dict_advantage = 0
                total_zstd_dict += std_size

            print(f"    {jsonl_path.name[:40]:40s} {format_size(original_size):>10s} → "
                  f"std:{std_ratio:5.1f}% dict:{dict_ratio:5.1f}% (+{dict_advantage:+.1f}%p)")

            results["sessions"].append({
                "file": jsonl_path.name,
                "original": original_size,
                "zstd_standard": std_size,
                "zstd_dict": dict_size if session_dict else std_size,
            })

        except Exception as e:
            print(f"    {jsonl_path.name}: Error - {e}")

    # 3. MASC 파일 분석
    print(f"\n[2] MASC FILES: {masc_dir}")
    print("-" * 50)

    if masc_dir.exists():
        masc_files = list(masc_dir.rglob("*.json")) + list(masc_dir.rglob("*.md"))
        masc_total_original = 0
        masc_total_zstd = 0

        for masc_path in masc_files[:10]:
            try:
                data = masc_path.read_bytes()
                original_size = len(data)
                masc_total_original += original_size

                cctx = zstd.ZstdCompressor(level=3)
                compressed = cctx.compress(data)
                compressed_size = len(compressed)
                masc_total_zstd += compressed_size

                ratio = (1 - compressed_size / original_size) * 100

                rel_path = masc_path.relative_to(masc_dir)
                print(f"    {str(rel_path)[:40]:40s} {format_size(original_size):>10s} → {ratio:5.1f}%")

            except Exception as e:
                print(f"    {masc_path.name}: Error - {e}")

        print(f"\n    MASC Total: {format_size(masc_total_original)} → {format_size(masc_total_zstd)} "
              f"({(1 - masc_total_zstd/masc_total_original)*100:.1f}%)")

    # 4. 전체 통계
    print("\n" + "=" * 70)
    print("SUMMARY")
    print("=" * 70)

    std_ratio = (1 - total_zstd / total_original) * 100
    dict_ratio = (1 - total_zstd_dict / total_original) * 100 if session_dict else std_ratio
    savings_std = total_original - total_zstd
    savings_dict = total_original - total_zstd_dict

    print(f"""
┌─────────────────────────────────────────────────────────────────┐
│  REAL SECOND BRAIN COMPRESSION RESULTS                         │
├─────────────────────────────────────────────────────────────────┤
│  Session Files Tested:     {len(results['sessions']):3d} files                           │
│  Original Size:            {format_size(total_original):>10s}                         │
├─────────────────────────────────────────────────────────────────┤
│  Standard zstd:            {format_size(total_zstd):>10s}  ({std_ratio:5.1f}% compression)    │
│  Savings:                  {format_size(savings_std):>10s}                          │
├─────────────────────────────────────────────────────────────────┤
│  Dictionary zstd:          {format_size(total_zstd_dict):>10s}  ({dict_ratio:5.1f}% compression)    │
│  Savings:                  {format_size(savings_dict):>10s}                          │
│  Dictionary Advantage:     {dict_ratio - std_ratio:+5.1f}%p                           │
└─────────────────────────────────────────────────────────────────┘
""")

    # 전체 1.7GB 추정
    all_files = list(session_dir.glob("*.jsonl"))
    total_all = sum(f.stat().st_size for f in all_files)

    estimated_savings_std = total_all * (std_ratio / 100)
    estimated_savings_dict = total_all * (dict_ratio / 100)

    print(f"""
┌─────────────────────────────────────────────────────────────────┐
│  ESTIMATED SAVINGS (ALL {len(all_files)} FILES, {format_size(total_all):>7s} total)         │
├─────────────────────────────────────────────────────────────────┤
│  Standard zstd:            {format_size(int(estimated_savings_std)):>10s} savings               │
│  Dictionary zstd:          {format_size(int(estimated_savings_dict)):>10s} savings               │
└─────────────────────────────────────────────────────────────────┘
""")

    # 5. Dashboard용 JSON 저장
    dashboard_data = {
        "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ"),
        "files_tested": len(results["sessions"]),
        "total_files": len(all_files),
        "original_bytes": total_original,
        "zstd_standard_bytes": total_zstd,
        "zstd_dict_bytes": total_zstd_dict,
        "compression_ratio_std": round(std_ratio, 2),
        "compression_ratio_dict": round(dict_ratio, 2),
        "dict_advantage_pct": round(dict_ratio - std_ratio, 2),
        "estimated_total_bytes": total_all,
        "estimated_savings_std": int(estimated_savings_std),
        "estimated_savings_dict": int(estimated_savings_dict),
    }

    output_path = Path(__file__).parent.parent / "data" / "compression_dashboard.json"
    output_path.parent.mkdir(exist_ok=True)
    with open(output_path, "w") as f:
        json.dump(dashboard_data, f, indent=2)

    print(f"Dashboard data saved to: {output_path}")

if __name__ == "__main__":
    main()
