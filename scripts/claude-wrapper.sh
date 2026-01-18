#!/bin/bash
# Claude CLI wrapper to avoid FSWatcher socket file issues
# Sets TMPDIR before Node.js starts to prevent caching issues

export TMPDIR="/tmp/claude-safe"
mkdir -p "$TMPDIR"

# Debug logging (stderr to avoid interfering with stdout capture)
echo "[wrapper] $(date): Starting claude with args: $@" >> /tmp/claude-wrapper.log

# Run Claude with stdin from /dev/null to prevent waiting for input
exec claude "$@" < /dev/null
