#!/usr/bin/env python3
"""
collect_samples.py - Collect LLM response samples for dictionary training

Usage:
    python collect_samples.py --type json --count 200 --output ./data/samples/json/
    python collect_samples.py --type code --count 200 --output ./data/samples/code/
    python collect_samples.py --type all --count 500 --output ./data/samples/

Requirements:
    - Ollama running locally (default LLM backend)
    - OR set ANTHROPIC_API_KEY for Claude
"""

import argparse
import json
import os
import subprocess
import sys
import hashlib
from pathlib import Path
from typing import Iterator

# Sample prompts by content type
PROMPTS = {
    "json": [
        "Return a JSON object with user profile data including name, email, preferences",
        "Generate JSON for an API response with status, data array, and pagination",
        "Create a JSON config for a web application with database and cache settings",
        "Return JSON for a product catalog with items, categories, and prices",
        "Generate a JSON error response with code, message, and details",
    ],
    "code": [
        "Write a Python function to parse CSV files with error handling",
        "Create a TypeScript React component for a login form",
        "Write an OCaml function to find the longest increasing subsequence",
        "Create a Go function for concurrent API requests with rate limiting",
        "Write a Rust function to parse command line arguments",
    ],
    "markdown": [
        "Write documentation for a REST API endpoint with examples",
        "Create a README for an open source project with installation steps",
        "Write a tutorial on setting up Docker containers",
        "Create release notes for a software update",
        "Write a technical specification for a database schema",
    ],
    "mixed": [
        "Explain how to implement a cache with code examples",
        "Describe the architecture of a web application with diagrams",
        "Create a troubleshooting guide with commands and explanations",
        "Write about best practices for error handling with examples",
        "Explain async programming with code snippets",
    ],
}


def ollama_generate(prompt: str, model: str = "llama3.2") -> str:
    """Generate response using Ollama"""
    try:
        result = subprocess.run(
            ["ollama", "run", model, prompt],
            capture_output=True,
            text=True,
            timeout=60,
        )
        return result.stdout.strip()
    except (subprocess.TimeoutExpired, FileNotFoundError) as e:
        print(f"Ollama error: {e}", file=sys.stderr)
        return ""


def generate_samples(content_type: str, count: int) -> Iterator[str]:
    """Generate samples of given content type"""
    prompts = PROMPTS.get(content_type, PROMPTS["mixed"])
    generated = 0

    while generated < count:
        for prompt in prompts:
            if generated >= count:
                break

            # Add variation to prompts
            variation = f"{prompt} (variation {generated % 10})"
            response = ollama_generate(variation)

            if response and len(response) > 100:
                yield response
                generated += 1
                print(f"Generated {generated}/{count} samples", end="\r")

    print()


def main():
    parser = argparse.ArgumentParser(description="Collect LLM samples for dictionary training")
    parser.add_argument("--type", choices=["json", "code", "markdown", "mixed", "all"],
                        default="all", help="Content type to collect")
    parser.add_argument("--count", type=int, default=200,
                        help="Number of samples per type (default: 200)")
    parser.add_argument("--output", type=str, default="./data/samples",
                        help="Output directory")
    parser.add_argument("--model", type=str, default="llama3.2",
                        help="Ollama model to use")

    args = parser.parse_args()

    types = ["json", "code", "markdown", "mixed"] if args.type == "all" else [args.type]

    for content_type in types:
        output_dir = Path(args.output) / content_type
        output_dir.mkdir(parents=True, exist_ok=True)

        print(f"\nCollecting {args.count} {content_type} samples...")

        for i, sample in enumerate(generate_samples(content_type, args.count)):
            # Create unique filename from content hash
            hash_prefix = hashlib.md5(sample.encode()).hexdigest()[:8]
            filename = output_dir / f"sample_{i:04d}_{hash_prefix}.txt"

            with open(filename, "w") as f:
                f.write(sample)

        print(f"Saved {args.count} samples to {output_dir}")

    print("\nDone! Now train dictionaries with:")
    for t in types:
        print(f"  dune exec train_dictionary -- --type {t} --samples-dir {args.output}/{t} --output ./data/{t}.zdict")


if __name__ == "__main__":
    main()
