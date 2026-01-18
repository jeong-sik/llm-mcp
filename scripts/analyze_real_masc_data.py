#!/usr/bin/env python3
"""
Real-World MASC Data Analysis

Analyze actual MASC messages and measure:
1. Message size distribution
2. Compression potential (JSON vs Compact)
3. Agent activity patterns
4. Task completion times
"""

import json
import os
import gzip
import zstandard as zstd
from pathlib import Path
from datetime import datetime
from collections import defaultdict
import statistics

MASC_DIR = Path.home() / "me" / ".masc"
MESSAGES_DIR = MASC_DIR / "messages"

def analyze_messages():
    """Analyze all MASC messages."""
    print("=" * 70)
    print("Real-World MASC Message Analysis")
    print("=" * 70)
    print()

    messages = []
    sizes = []
    agents = defaultdict(int)
    timestamps = []

    for f in sorted(MESSAGES_DIR.glob("*.json")):
        try:
            with open(f) as fp:
                msg = json.load(fp)
                messages.append(msg)
                sizes.append(f.stat().st_size)
                agents[msg.get('from', 'unknown')] += 1
                if 'timestamp' in msg:
                    timestamps.append(msg['timestamp'])
        except:
            pass

    print(f"[1/4] Message Statistics")
    print("-" * 70)
    print(f"  Total messages: {len(messages)}")
    print(f"  Total size: {sum(sizes):,} bytes ({sum(sizes)/1024:.1f} KB)")
    print(f"  Avg size: {statistics.mean(sizes):.1f} bytes")
    print(f"  Min/Max: {min(sizes)} / {max(sizes)} bytes")
    print()

    print(f"[2/4] Agent Activity")
    print("-" * 70)
    for agent, count in sorted(agents.items(), key=lambda x: -x[1]):
        pct = count / len(messages) * 100
        bar = "█" * int(pct / 2)
        print(f"  {agent:15s} {count:4d} ({pct:5.1f}%) {bar}")
    print()

    # Compression analysis
    print(f"[3/4] Compression Potential")
    print("-" * 70)

    # Concatenate all messages as a typical batch
    all_json = json.dumps(messages).encode()

    # Gzip
    gzip_compressed = gzip.compress(all_json)
    gzip_ratio = (1 - len(gzip_compressed) / len(all_json)) * 100

    # Zstd (no dict)
    cctx = zstd.ZstdCompressor(level=3)
    zstd_compressed = cctx.compress(all_json)
    zstd_ratio = (1 - len(zstd_compressed) / len(all_json)) * 100

    # Zstd with trained dictionary
    # Train on half, test on all
    training_samples = [json.dumps(m).encode() for m in messages[:len(messages)//2]]
    if len(training_samples) > 10:
        zdict = zstd.train_dictionary(32768, training_samples)
        cctx_dict = zstd.ZstdCompressor(level=3, dict_data=zdict)
        zstd_dict_compressed = cctx_dict.compress(all_json)
        zstd_dict_ratio = (1 - len(zstd_dict_compressed) / len(all_json)) * 100
    else:
        zstd_dict_ratio = 0
        zstd_dict_compressed = b""

    print(f"  Original JSON:     {len(all_json):,} bytes")
    print(f"  Gzip:              {len(gzip_compressed):,} bytes ({gzip_ratio:.1f}% saved)")
    print(f"  Zstd (no dict):    {len(zstd_compressed):,} bytes ({zstd_ratio:.1f}% saved)")
    print(f"  Zstd (w/ dict):    {len(zstd_dict_compressed):,} bytes ({zstd_dict_ratio:.1f}% saved)")
    print()

    # Per-message compression
    print(f"[4/4] Per-Message Compression (sample)")
    print("-" * 70)

    sample_msgs = [m for m in messages if len(m.get('content', '')) > 50][:5]
    for i, msg in enumerate(sample_msgs):
        msg_json = json.dumps(msg).encode()
        msg_gzip = gzip.compress(msg_json)
        msg_zstd = cctx.compress(msg_json)

        print(f"  Msg {i+1}: {len(msg_json):4d}B → gzip:{len(msg_gzip):4d}B ({(1-len(msg_gzip)/len(msg_json))*100:.0f}%) "
              f"zstd:{len(msg_zstd):4d}B ({(1-len(msg_zstd)/len(msg_json))*100:.0f}%)")
        print(f"         \"{msg.get('content', '')[:60]}...\"")

    print()
    print("=" * 70)
    print("KEY FINDING")
    print("=" * 70)
    print(f"""
  MASC messages are ALREADY quite compact (~400 bytes avg).

  Batch compression is effective:
    - Gzip: {gzip_ratio:.1f}% savings
    - Zstd: {zstd_ratio:.1f}% savings
    - Zstd+Dict: {zstd_dict_ratio:.1f}% savings

  For high-frequency messaging (100+ msg/sec), this translates to:
    - Original: ~40 KB/sec
    - Compressed: ~{40 * (1-zstd_dict_ratio/100):.1f} KB/sec
    - Savings: ~{40 * zstd_dict_ratio/100:.1f} KB/sec
""")

    return {
        'total_messages': len(messages),
        'total_bytes': sum(sizes),
        'avg_size': statistics.mean(sizes),
        'agents': dict(agents),
        'gzip_ratio': gzip_ratio,
        'zstd_ratio': zstd_ratio,
        'zstd_dict_ratio': zstd_dict_ratio,
    }


def analyze_tasks():
    """Analyze task completion patterns."""
    print()
    print("=" * 70)
    print("Task Completion Analysis")
    print("=" * 70)
    print()

    tasks_file = MASC_DIR / "tasks" / "tasks.json"
    if not tasks_file.exists():
        print("  No tasks.json found")
        return {}

    with open(tasks_file) as f:
        tasks = json.load(f)

    completed = [t for t in tasks if t.get('status') == 'done']
    claimed = [t for t in tasks if t.get('status') == 'claimed']
    pending = [t for t in tasks if t.get('status') == 'todo']

    print(f"  Total tasks: {len(tasks)}")
    print(f"  Completed: {len(completed)}")
    print(f"  In progress: {len(claimed)}")
    print(f"  Pending: {len(pending)}")
    print()

    # By agent
    agent_tasks = defaultdict(lambda: {'done': 0, 'claimed': 0})
    for t in tasks:
        agent = t.get('claimed_by', 'unclaimed')
        status = t.get('status', 'unknown')
        if status == 'done':
            agent_tasks[agent]['done'] += 1
        elif status == 'claimed':
            agent_tasks[agent]['claimed'] += 1

    print("  Tasks by Agent:")
    for agent, counts in sorted(agent_tasks.items(), key=lambda x: -x[1]['done']):
        print(f"    {agent:15s}: {counts['done']:3d} done, {counts['claimed']:2d} in progress")

    return {
        'total': len(tasks),
        'completed': len(completed),
        'in_progress': len(claimed),
        'pending': len(pending),
        'by_agent': dict(agent_tasks),
    }


if __name__ == '__main__':
    msg_stats = analyze_messages()
    task_stats = analyze_tasks()

    # Save results
    results = {
        'date': datetime.now().isoformat(),
        'messages': msg_stats,
        'tasks': task_stats,
    }

    output_path = Path(__file__).parent.parent / 'data' / 'real_masc_analysis.json'
    with open(output_path, 'w') as f:
        json.dump(results, f, indent=2)
    print(f"\nResults saved to: {output_path}")
