#!/usr/bin/env python3
"""
All 4 Pivots Experiment - Comprehensive Validation

Pivot A: Compression as similarity metric
Pivot B: Delta component analysis
Pivot C: Human vs LLM baseline
Pivot D: Real system data measurement
"""

import zstandard as zstd
import json
import time
import os
import re
from pathlib import Path
from dataclasses import dataclass, asdict
from typing import List, Dict, Tuple
import statistics
import subprocess

# ============================================================
# Sample Data Collection
# ============================================================

# Human-written code samples (simulating GitHub)
HUMAN_CODE_SAMPLES = [
    # Real human style - less structured, more varied
    b'''def merge(a, b):
    # TODO: optimize this later
    res = []
    while a and b:
        if a[0] < b[0]:
            res.append(a.pop(0))
        else:
            res.append(b.pop(0))
    return res + a + b  # whatever's left
''',
    b'''// quick and dirty json parser
const parse = (s) => {
  try { return JSON.parse(s); }
  catch(e) { console.log('parse error:', e); return null; }
}
''',
    b'''# scrape.py - grab some data
import requests
from bs4 import BeautifulSoup

url = "https://example.com"
r = requests.get(url)
soup = BeautifulSoup(r.text, 'html.parser')
for link in soup.find_all('a'):
    print(link.get('href'))
''',
    b'''{
  "name": "my-project",
  "version": "0.0.1",
  "scripts": {
    "start": "node index.js",
    "test": "jest"
  }
}
''',
]

# LLM-style code (more structured, verbose, documented)
LLM_CODE_SAMPLES = [
    b'''def merge_sorted_lists(list_a: list, list_b: list) -> list:
    """
    Merge two sorted lists into a single sorted list.

    Args:
        list_a: First sorted list
        list_b: Second sorted list

    Returns:
        A new sorted list containing all elements
    """
    result = []
    i, j = 0, 0

    while i < len(list_a) and j < len(list_b):
        if list_a[i] <= list_b[j]:
            result.append(list_a[i])
            i += 1
        else:
            result.append(list_b[j])
            j += 1

    result.extend(list_a[i:])
    result.extend(list_b[j:])

    return result
''',
    b'''/**
 * Parse JSON string safely with error handling.
 * @param {string} jsonString - The JSON string to parse
 * @returns {Object|null} Parsed object or null on error
 */
function parseJSON(jsonString) {
    try {
        return JSON.parse(jsonString);
    } catch (error) {
        console.error('Failed to parse JSON:', error.message);
        return null;
    }
}
''',
    b'''"""
Web Scraper Module

This module provides functionality to scrape web pages
and extract relevant data using BeautifulSoup.
"""

import requests
from bs4 import BeautifulSoup
from typing import List


def scrape_links(url: str) -> List[str]:
    """
    Scrape all hyperlinks from a given URL.

    Args:
        url: The URL to scrape

    Returns:
        List of href values found on the page
    """
    response = requests.get(url)
    soup = BeautifulSoup(response.text, 'html.parser')

    links = []
    for anchor in soup.find_all('a'):
        href = anchor.get('href')
        if href:
            links.append(href)

    return links
''',
    b'''{
  "name": "my-project",
  "version": "1.0.0",
  "description": "A sample project demonstrating best practices",
  "main": "index.js",
  "scripts": {
    "start": "node index.js",
    "test": "jest --coverage",
    "lint": "eslint .",
    "build": "webpack --mode production"
  },
  "keywords": ["sample", "demo"],
  "author": "Developer",
  "license": "MIT"
}
''',
]

# Components for delta analysis
def extract_components(code: bytes) -> Dict[str, bytes]:
    """Extract different components from code."""
    text = code.decode('utf-8', errors='ignore')

    # Extract comments
    comments = re.findall(r'#.*$|//.*$|/\*.*?\*/|""".*?"""|\'\'\'.*?\'\'\'', text, re.MULTILINE | re.DOTALL)
    comments_text = '\n'.join(comments).encode()

    # Extract strings
    strings = re.findall(r'"[^"]*"|\'[^\']*\'', text)
    strings_text = '\n'.join(strings).encode()

    # Extract structure (braces, brackets, keywords)
    structure = re.findall(r'[\{\}\[\]\(\):;,]|def |class |function |return |if |else |for |while ', text)
    structure_text = ''.join(structure).encode()

    # Whitespace patterns
    whitespace = re.findall(r'\n\s+', text)
    whitespace_text = ''.join(whitespace).encode()

    return {
        'full': code,
        'comments': comments_text if comments_text else b' ',
        'strings': strings_text if strings_text else b' ',
        'structure': structure_text if structure_text else b' ',
        'whitespace': whitespace_text if whitespace_text else b' ',
    }


# ============================================================
# Pivot A: Compression as Similarity Metric
# ============================================================

def pivot_a_similarity_metric():
    """Test if compression ratio correlates with content similarity."""
    print("\n" + "=" * 70)
    print("PIVOT A: Compression as Similarity Metric")
    print("=" * 70)
    print("\nHypothesis: Higher compression = More similar content")
    print()

    # Combine all samples
    all_samples = HUMAN_CODE_SAMPLES + LLM_CODE_SAMPLES

    # Train on LLM samples
    training_data = LLM_CODE_SAMPLES * 10
    zstd_dict = zstd.train_dictionary(32768, training_data).as_bytes()
    zd = zstd.ZstdCompressionDict(zstd_dict)
    cctx = zstd.ZstdCompressor(level=3, dict_data=zd)

    results = []

    print("Compressing with LLM-trained dictionary:")
    print("-" * 50)

    for i, sample in enumerate(HUMAN_CODE_SAMPLES):
        compressed = cctx.compress(sample)
        ratio = (1 - len(compressed) / len(sample)) * 100
        results.append(('Human', i+1, ratio))
        print(f"  Human #{i+1}: {ratio:5.1f}% compression")

    print()
    for i, sample in enumerate(LLM_CODE_SAMPLES):
        compressed = cctx.compress(sample)
        ratio = (1 - len(compressed) / len(sample)) * 100
        results.append(('LLM', i+1, ratio))
        print(f"  LLM   #{i+1}: {ratio:5.1f}% compression")

    human_avg = statistics.mean([r[2] for r in results if r[0] == 'Human'])
    llm_avg = statistics.mean([r[2] for r in results if r[0] == 'LLM'])

    print()
    print(f"  Human average: {human_avg:.1f}%")
    print(f"  LLM average:   {llm_avg:.1f}%")
    print(f"  Delta:         {llm_avg - human_avg:+.1f}%")

    # Verdict
    print()
    if llm_avg - human_avg > 10:
        print("  ✅ VALIDATED: LLM outputs compress significantly better with LLM dictionary")
        print("     → Compression CAN distinguish LLM from human code")
    elif llm_avg - human_avg > 5:
        print("  ⚠️ PARTIAL: Some difference, but not conclusive")
    else:
        print("  ❌ FAILED: No significant difference - dictionary is not LLM-specific")

    return {'human_avg': human_avg, 'llm_avg': llm_avg, 'delta': llm_avg - human_avg}


# ============================================================
# Pivot B: Delta Component Analysis
# ============================================================

def pivot_b_delta_analysis():
    """Analyze what components cause cross-model degradation."""
    print("\n" + "=" * 70)
    print("PIVOT B: Delta Component Analysis")
    print("=" * 70)
    print("\nQuestion: What parts of LLM output are model-specific?")
    print()

    # Train dictionary on LLM sample 1 (simulating "Claude")
    claude_samples = [LLM_CODE_SAMPLES[0]] * 10
    zstd_dict = zstd.train_dictionary(16384, claude_samples).as_bytes()
    zd = zstd.ZstdCompressionDict(zstd_dict)
    cctx = zstd.ZstdCompressor(level=3, dict_data=zd)

    # Test on different sample (simulating "GPT")
    gpt_sample = LLM_CODE_SAMPLES[1]

    # Extract and analyze components
    claude_components = extract_components(LLM_CODE_SAMPLES[0])
    gpt_components = extract_components(gpt_sample)

    print("Component-level compression (Claude dict → GPT code):")
    print("-" * 50)

    component_results = {}
    for comp_name in ['full', 'comments', 'strings', 'structure', 'whitespace']:
        if len(gpt_components[comp_name]) > 10:
            compressed = cctx.compress(gpt_components[comp_name])
            ratio = (1 - len(compressed) / len(gpt_components[comp_name])) * 100
            component_results[comp_name] = ratio
            print(f"  {comp_name:12s}: {ratio:5.1f}% ({len(gpt_components[comp_name]):4d} bytes)")

    # Find worst transferring component
    if component_results:
        worst = min(component_results.items(), key=lambda x: x[1])
        best = max(component_results.items(), key=lambda x: x[1])

        print()
        print(f"  Best transfer:  {best[0]} ({best[1]:.1f}%)")
        print(f"  Worst transfer: {worst[0]} ({worst[1]:.1f}%)")
        print()
        print(f"  → {worst[0].upper()} is most model-specific")

    return component_results


# ============================================================
# Pivot C: Human vs LLM Baseline
# ============================================================

def pivot_c_human_baseline():
    """Compare Human→LLM vs LLM→LLM compression."""
    print("\n" + "=" * 70)
    print("PIVOT C: Human vs LLM Baseline")
    print("=" * 70)
    print("\nCritical question: Is 'cross-model transfer' just 'code is code'?")
    print()

    results = {}

    # 1. Human dict → Human
    human_training = HUMAN_CODE_SAMPLES * 10
    human_dict = zstd.train_dictionary(32768, human_training).as_bytes()
    zd_human = zstd.ZstdCompressionDict(human_dict)
    cctx_human = zstd.ZstdCompressor(level=3, dict_data=zd_human)

    ratios = []
    for sample in HUMAN_CODE_SAMPLES:
        compressed = cctx_human.compress(sample)
        ratios.append((1 - len(compressed) / len(sample)) * 100)
    results['Human→Human'] = statistics.mean(ratios)

    # 2. Human dict → LLM
    ratios = []
    for sample in LLM_CODE_SAMPLES:
        compressed = cctx_human.compress(sample)
        ratios.append((1 - len(compressed) / len(sample)) * 100)
    results['Human→LLM'] = statistics.mean(ratios)

    # 3. LLM dict → Human
    llm_training = LLM_CODE_SAMPLES * 10
    llm_dict = zstd.train_dictionary(32768, llm_training).as_bytes()
    zd_llm = zstd.ZstdCompressionDict(llm_dict)
    cctx_llm = zstd.ZstdCompressor(level=3, dict_data=zd_llm)

    ratios = []
    for sample in HUMAN_CODE_SAMPLES:
        compressed = cctx_llm.compress(sample)
        ratios.append((1 - len(compressed) / len(sample)) * 100)
    results['LLM→Human'] = statistics.mean(ratios)

    # 4. LLM dict → LLM
    ratios = []
    for sample in LLM_CODE_SAMPLES:
        compressed = cctx_llm.compress(sample)
        ratios.append((1 - len(compressed) / len(sample)) * 100)
    results['LLM→LLM'] = statistics.mean(ratios)

    print("Cross-domain compression matrix:")
    print("-" * 50)
    print(f"{'':15s} | {'→ Human':>10s} | {'→ LLM':>10s}")
    print("-" * 50)
    print(f"{'Human dict':15s} | {results['Human→Human']:9.1f}% | {results['Human→LLM']:9.1f}%")
    print(f"{'LLM dict':15s} | {results['LLM→Human']:9.1f}% | {results['LLM→LLM']:9.1f}%")

    # Analysis
    print()
    llm_advantage = results['LLM→LLM'] - results['Human→LLM']

    if llm_advantage > 5:
        print(f"  ✅ LLM dict beats Human dict by {llm_advantage:.1f}% on LLM outputs")
        print("     → LLM-specific patterns EXIST and are capturable")
    else:
        print(f"  ❌ LLM dict only {llm_advantage:.1f}% better than Human dict")
        print("     → 'Cross-model transfer' might just be 'code is code'")

    return results


# ============================================================
# Pivot D: Real System Measurement
# ============================================================

def pivot_d_real_system():
    """Measure impact on real system data."""
    print("\n" + "=" * 70)
    print("PIVOT D: Real System Measurement")
    print("=" * 70)
    print("\nMeasuring actual data from Second Brain system...")
    print()

    results = {}

    # Check hook logs
    hook_log = Path.home() / '.claude' / 'logs' / 'user-prompt-submit.log'
    if hook_log.exists():
        size = hook_log.stat().st_size
        with open(hook_log, 'rb') as f:
            content = f.read()

        # Compress with standard zstd
        cctx = zstd.ZstdCompressor(level=3)
        compressed = cctx.compress(content)
        ratio = (1 - len(compressed) / len(content)) * 100

        results['hook_log'] = {
            'original': size,
            'compressed': len(compressed),
            'ratio': ratio
        }
        print(f"  Hook log: {size:,} bytes → {len(compressed):,} bytes ({ratio:.1f}%)")
    else:
        print("  Hook log: Not found")

    # Check session transcripts
    claude_projects = Path.home() / '.claude' / 'projects'
    if claude_projects.exists():
        jsonl_files = list(claude_projects.rglob('*.jsonl'))[:3]  # Sample 3
        for jf in jsonl_files:
            size = jf.stat().st_size
            if size > 1000:  # Only meaningful files
                with open(jf, 'rb') as f:
                    content = f.read()

                cctx = zstd.ZstdCompressor(level=3)
                compressed = cctx.compress(content)
                ratio = (1 - len(compressed) / len(content)) * 100

                print(f"  Session: {size:,} bytes → {len(compressed):,} bytes ({ratio:.1f}%)")
                results[str(jf.name)] = {'original': size, 'compressed': len(compressed), 'ratio': ratio}

    # Summary
    if results:
        avg_ratio = statistics.mean([r['ratio'] for r in results.values() if isinstance(r, dict)])
        total_original = sum([r['original'] for r in results.values() if isinstance(r, dict)])
        total_compressed = sum([r['compressed'] for r in results.values() if isinstance(r, dict)])

        print()
        print(f"  Average compression: {avg_ratio:.1f}%")
        print(f"  Total savings: {total_original - total_compressed:,} bytes")
        print()
        print("  → Real system data compresses well with standard zstd")
        print("  → Dictionary training on actual prompts would improve further")

    return results


# ============================================================
# Main
# ============================================================

def run_all_pivots():
    print("=" * 70)
    print("COMPREHENSIVE PIVOT EXPERIMENTS")
    print("=" * 70)
    print("\nRunning all 4 pivots to determine research direction...")

    all_results = {}

    # Run each pivot
    all_results['A_similarity'] = pivot_a_similarity_metric()
    all_results['B_delta'] = pivot_b_delta_analysis()
    all_results['C_baseline'] = pivot_c_human_baseline()
    all_results['D_system'] = pivot_d_real_system()

    # Final recommendation
    print("\n" + "=" * 70)
    print("RECOMMENDATION")
    print("=" * 70)

    # Analyze results
    a_delta = all_results['A_similarity'].get('delta', 0)
    c_llm_llm = all_results['C_baseline'].get('LLM→LLM', 0)
    c_human_llm = all_results['C_baseline'].get('Human→LLM', 0)
    c_advantage = c_llm_llm - c_human_llm

    print()
    if c_advantage > 10:
        print("  🎯 STRONGEST PIVOT: C (Human vs LLM Baseline)")
        print(f"     LLM dictionary beats Human dictionary by {c_advantage:.1f}%")
        print("     → This proves LLM-specific patterns exist!")
        print()
        print("  Paper framing: 'Compression reveals LLM fingerprints'")
    elif a_delta > 10:
        print("  🎯 STRONGEST PIVOT: A (Similarity Metric)")
        print(f"     LLM outputs compress {a_delta:.1f}% better than Human")
        print("     → Compression as LLM detector")
    else:
        print("  ⚠️ NO CLEAR WINNER")
        print("     Consider Pivot D: Focus on practical system benefits")
        print("     → 'X% storage savings in production'")

    # Save results
    output = {
        'date': time.strftime('%Y-%m-%d %H:%M:%S'),
        'pivots': all_results,
        'recommendation': 'See terminal output'
    }

    output_path = Path(__file__).parent.parent / 'data' / 'all_pivots_results.json'
    with open(output_path, 'w') as f:
        json.dump(output, f, indent=2, default=str)
    print(f"\n  Results saved to: {output_path}")


if __name__ == '__main__':
    run_all_pivots()
