#!/usr/bin/env bash
# Clojure guardian - enforces /clojure skill before editing Clojure files
#
# Rules:
#   - Block Edit/Write on .clj/.cljc/.cljs/deps.edn until /clojure skill invoked
#   - Soft reminder when Reading Clojure files

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FLAG_FILE="${CLAUDE_PROJECT_DIR:-.}/.claude/.clojure-ready"

input=$(cat)
tool_name=$(echo "$input" | jq -r '.tool_name // empty' 2>/dev/null || true)

# Track /clojure skill invocation
if [[ "$tool_name" == "Skill" ]]; then
    skill=$(echo "$input" | jq -r '.tool_input.skill // empty' 2>/dev/null || true)
    [[ "$skill" == "clojure" ]] && touch "$FLAG_FILE"
    exit 0
fi

# Get file path for Edit/Write/Read
file_path=""
if [[ "$tool_name" == "Edit" || "$tool_name" == "Write" || "$tool_name" == "Read" ]]; then
    file_path=$(echo "$input" | jq -r '.tool_input.file_path // empty' 2>/dev/null || true)
    # Only apply to Clojure files
    [[ "$file_path" =~ \.(clj|cljc|cljs)$ || "$file_path" =~ deps\.edn$ ]] || exit 0
fi

# Read: soft reminder
if [[ "$tool_name" == "Read" && -n "$file_path" ]]; then
    [[ -f "$FLAG_FILE" ]] && exit 0
    cat << 'EOF'
{"hookSpecificOutput":{"hookEventName":"PreToolUse","additionalContext":"Consider invoking /clojure skill first for REPL-driven exploration."}}
EOF
    exit 0
fi

# Edit/Write: block until skill invoked
if [[ "$tool_name" == "Edit" || "$tool_name" == "Write" ]]; then
    [[ -f "$FLAG_FILE" ]] && exit 0
    cat << 'EOF'
{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"Run /clojure first. This loads Clojure coding guidelines and REPL workflow."}}
EOF
    exit 0
fi
