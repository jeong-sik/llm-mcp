#!/usr/bin/env python3
"""
chain-heartbeat.py

Periodic checks driven by llm-mcp `chain.run` over MCP JSON-RPC HTTP (`tools/call`).

This is intentionally a separate daemon-like process:
  - Keeps llm-mcp stateless per request.
  - Easy to run under launchd/systemd/cron.
  - Produces JSONL logs suitable for alerting/trend analysis.

Stdlib only.
"""

from __future__ import annotations

import argparse
import json
import os
import random
import sys
import time
import urllib.error
import urllib.request
from typing import Any


DEFAULT_MCP_URL = os.getenv("LLM_MCP_URL", "http://localhost:8932/mcp")
DEFAULT_MCP_API_KEY = os.getenv("LLM_MCP_API_KEY", "") or os.getenv("MCP_API_KEY", "")
DEFAULT_INTERVAL_S = int(os.getenv("LLM_MCP_CHAIN_HEARTBEAT_INTERVAL_S", "300"))
DEFAULT_LOCK = os.getenv("LLM_MCP_CHAIN_HEARTBEAT_LOCK", "/tmp/llm-mcp-chain-heartbeat.lock")
DEFAULT_LOG = os.getenv(
    "LLM_MCP_CHAIN_HEARTBEAT_LOG",
    os.path.join(os.path.expanduser("~"), "me", "logs", "llm-mcp-chain-heartbeat.jsonl"),
)


def now_iso() -> str:
    return time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())


def expand_path(path: str) -> str:
    return os.path.expanduser(path)


def ensure_parent_dir(path: str) -> None:
    parent = os.path.dirname(path)
    if parent and not os.path.exists(parent):
        os.makedirs(parent, exist_ok=True)


def _extract_error_message_from_body(body_text: str) -> str | None:
    try:
        j = json.loads(body_text)
    except json.JSONDecodeError:
        return None
    if not isinstance(j, dict):
        return None
    err = j.get("error")
    if isinstance(err, str):
        return err
    if isinstance(err, dict):
        msg = err.get("message")
        if isinstance(msg, str):
            return msg
    msg = j.get("message")
    return msg if isinstance(msg, str) else None


def post_json(url: str, payload: dict, timeout_s: int = 60, headers_extra: dict | None = None) -> dict:
    data = json.dumps(payload).encode("utf-8")
    headers = {"Content-Type": "application/json"}
    if isinstance(headers_extra, dict):
        headers.update(headers_extra)
    req = urllib.request.Request(url, data=data, headers=headers, method="POST")
    try:
        with urllib.request.urlopen(req, timeout=timeout_s) as resp:
            body_bytes = resp.read()
    except urllib.error.HTTPError as e:
        try:
            body_text = e.read().decode("utf-8", errors="replace")
        except Exception:
            body_text = ""
        msg = _extract_error_message_from_body(body_text)
        if msg:
            raise RuntimeError(msg)
        truncated = body_text[:2000]
        raise RuntimeError(f"HTTP {e.code} {e.reason}: {truncated}")
    except urllib.error.URLError as e:
        raise RuntimeError(f"failed to reach MCP server: {e.reason}")

    body_text = body_bytes.decode("utf-8", errors="replace")
    try:
        j = json.loads(body_text)
    except json.JSONDecodeError:
        truncated = body_text[:2000]
        raise RuntimeError(f"MCP returned non-JSON response: {truncated}")
    if not isinstance(j, dict):
        raise RuntimeError("MCP returned unexpected JSON type")
    return j


def mcp_call_tool(mcp_url: str, name: str, args: dict, timeout_s: int = 120, mcp_api_key: str | None = None) -> dict:
    payload = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "tools/call",
        "params": {"name": name, "arguments": args},
    }
    headers_extra = {}
    if isinstance(mcp_api_key, str) and mcp_api_key.strip() != "":
        headers_extra["X-MCP-API-Key"] = mcp_api_key.strip()

    response = post_json(mcp_url, payload, timeout_s=timeout_s, headers_extra=headers_extra)
    err = response.get("error")
    if isinstance(err, dict):
        msg = err.get("message")
        if isinstance(msg, str):
            raise RuntimeError(msg)
        raise RuntimeError("Unknown MCP error")
    result = response.get("result")
    return result if isinstance(result, dict) else {}


def extract_text_content(result_obj: dict) -> str | None:
    if not isinstance(result_obj, dict):
        return None
    content = result_obj.get("content")
    if not isinstance(content, list) or not content:
        return None
    first = content[0]
    if not isinstance(first, dict):
        return None
    text = first.get("text")
    return text if isinstance(text, str) else None


def strip_extra_section(text: str) -> str:
    # llm-mcp appends tool "extra" as a human-readable block to the response text:
    # "\n\n[Extra]\n{...json...}".
    marker = "\n\n[Extra]\n"
    head, _sep, _tail = text.partition(marker)
    return head


def parse_json_maybe(text: str | None) -> dict | list | None:
    if not isinstance(text, str):
        return None
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        return None


def load_config(path: str) -> dict:
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)


def sanitize_for_log(value: Any) -> Any:
    if isinstance(value, dict):
        out: dict[str, Any] = {}
        for k, v in value.items():
            k_lower = k.lower()
            if k_lower in ("html", "token", "api_key", "apikey", "plugin_data") and isinstance(v, str):
                out[k] = f"<redacted len={len(v)}>"
            else:
                out[k] = sanitize_for_log(v)
        return out
    if isinstance(value, list):
        return [sanitize_for_log(v) for v in value]
    if isinstance(value, str) and len(value) > 200:
        return value[:200] + "...<truncated>"
    return value


def jsonl_append(path: str, record: dict) -> None:
    path = expand_path(path)
    ensure_parent_dir(path)
    line = json.dumps(record, ensure_ascii=False)
    with open(path, "a", encoding="utf-8") as f:
        f.write(line + "\n")


def is_pid_running(pid: int) -> bool:
    try:
        os.kill(pid, 0)
        return True
    except OSError:
        return False


def acquire_lock(lock_path: str, stale_after_s: int) -> None:
    lock_path = expand_path(lock_path)
    now = time.time()
    payload = {"pid": os.getpid(), "started_at": now}

    try:
        fd = os.open(lock_path, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o644)
        try:
            os.write(fd, json.dumps(payload).encode("utf-8"))
        finally:
            os.close(fd)
        return
    except FileExistsError:
        pass

    # Lock exists: check if it's stale.
    try:
        with open(lock_path, "r", encoding="utf-8") as f:
            existing = json.load(f)
        pid = int(existing.get("pid", 0))
        started_at = float(existing.get("started_at", 0))
    except Exception:
        pid = 0
        started_at = 0.0

    age = now - started_at if started_at > 0 else None
    if pid > 0 and is_pid_running(pid):
        raise RuntimeError(f"heartbeat already running (pid={pid})")

    if age is None or age > stale_after_s:
        # Stale lock: best-effort replace.
        try:
            os.unlink(lock_path)
        except Exception:
            raise RuntimeError(f"failed to remove stale lock: {lock_path}")
        acquire_lock(lock_path, stale_after_s=stale_after_s)
        return

    raise RuntimeError(f"heartbeat locked (age={age:.0f}s) - try again later")


def release_lock(lock_path: str) -> None:
    try:
        os.unlink(expand_path(lock_path))
    except Exception:
        pass


def _load_job_input(job: dict) -> dict:
    # 1) input_file: load JSON object
    input_file = job.get("input_file")
    if isinstance(input_file, str) and input_file.strip() != "":
        with open(expand_path(input_file), "r", encoding="utf-8") as f:
            data = json.load(f)
        if not isinstance(data, dict):
            raise ValueError("job.input_file must contain a JSON object")
        base = data
    else:
        base = job.get("input") or {}
        if not isinstance(base, dict):
            raise ValueError("job.input must be an object (or use input_file)")

    # 2) html_file shortcut: read file into input.html
    if "html" not in base:
        html_file = base.get("html_file")
        if isinstance(html_file, str) and html_file.strip() != "":
            with open(expand_path(html_file), "r", encoding="utf-8") as f:
                base = {**base, "html": f.read()}
    # Remove helper keys from final input
    if "html_file" in base:
        base = {k: v for (k, v) in base.items() if k != "html_file"}
    return base


def validate_jobs(config: dict) -> list[dict]:
    jobs = config.get("jobs")
    if not isinstance(jobs, list) or not jobs:
        raise ValueError("config.jobs must be a non-empty list")

    out: list[dict] = []
    for idx, job in enumerate(jobs):
        if not isinstance(job, dict):
            raise ValueError(f"config.jobs[{idx}] must be an object")
        enabled = job.get("enabled", True)
        if enabled is False:
            continue
        if enabled is not True:
            raise ValueError(f"config.jobs[{idx}].enabled must be a boolean")

        name = job.get("name") or f"job_{idx}"
        chain_id = job.get("chain_id")
        if not isinstance(chain_id, str) or chain_id.strip() == "":
            raise ValueError(f"config.jobs[{idx}].chain_id must be a non-empty string")

        out.append({**job, "name": name})
    return out


def run_job(mcp_url: str, job: dict, mcp_api_key: str | None) -> dict:
    name = job["name"]
    chain_id = job["chain_id"]
    started = time.time()

    timeout_s = int(job.get("timeout_s", 300))
    chain_input = _load_job_input(job)
    args = {
        "chain_id": chain_id,
        "input": chain_input,
        "trace": bool(job.get("trace", False)),
        "timeout": timeout_s,
    }

    raw_result = mcp_call_tool(mcp_url, "chain.run", args, timeout_s=timeout_s, mcp_api_key=mcp_api_key)
    text = extract_text_content(raw_result)
    if isinstance(text, str):
        text_no_extra = strip_extra_section(text)
    else:
        text_no_extra = None

    parsed = parse_json_maybe(text_no_extra)
    args_for_log = sanitize_for_log(args)
    parsed_for_log = sanitize_for_log(parsed)

    if isinstance(text, str) and len(text) > 4000:
        text = text[:4000] + "...<truncated>"

    verdict = "ok"
    if isinstance(parsed, dict) and "overall_passed" in parsed:
        verdict = "pass" if bool(parsed.get("overall_passed")) else "fail"

    duration_ms = int((time.time() - started) * 1000)
    return {
        "ts": now_iso(),
        "job": name,
        "chain_id": chain_id,
        "args": args_for_log,
        "duration_ms": duration_ms,
        "verdict": verdict,
        "result_text": text,
        "result_parsed": parsed_for_log,
    }


def main() -> int:
    ap = argparse.ArgumentParser(description="llm-mcp chain heartbeat loop")
    ap.add_argument("--mcp-url", default=DEFAULT_MCP_URL)
    ap.add_argument("--mcp-api-key", default=DEFAULT_MCP_API_KEY)
    ap.add_argument("--config", required=True, help="JSON config file (jobs list)")
    ap.add_argument("--interval-s", type=int, default=DEFAULT_INTERVAL_S)
    ap.add_argument("--jitter-s", type=int, default=5, help="Random jitter added to each sleep")
    ap.add_argument("--lock", default=DEFAULT_LOCK)
    ap.add_argument("--log", default=DEFAULT_LOG)
    ap.add_argument("--once", action="store_true")
    ap.add_argument("--stale-lock-after-s", type=int, default=3600)
    args = ap.parse_args()

    cfg = load_config(args.config)
    jobs = validate_jobs(cfg)

    acquire_lock(args.lock, stale_after_s=max(60, args.stale_lock_after_s))
    try:
        while True:
            cycle_ok = True
            cycle_started = time.time()
            for job in jobs:
                try:
                    rec = run_job(args.mcp_url, job, mcp_api_key=args.mcp_api_key)
                    jsonl_append(args.log, {**rec, "ok": True})
                    if rec.get("verdict") == "fail":
                        cycle_ok = False
                except Exception as e:
                    cycle_ok = False
                    jsonl_append(
                        args.log,
                        {
                            "ts": now_iso(),
                            "job": job.get("name", "unknown"),
                            "chain_id": job.get("chain_id"),
                            "ok": False,
                            "error": str(e),
                        },
                    )

            if args.once:
                return 0 if cycle_ok else 1

            elapsed = time.time() - cycle_started
            sleep_for = max(1, args.interval_s - int(elapsed))
            sleep_for += random.randint(0, max(0, args.jitter_s))
            time.sleep(sleep_for)
    finally:
        release_lock(args.lock)


if __name__ == "__main__":
    raise SystemExit(main())

