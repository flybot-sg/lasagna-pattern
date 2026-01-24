#!/usr/bin/env bash
# Guardian hook - enforces project rules before tool execution
# Rules:
#   1. /clojure skill before editing Clojure files
#   2. Review reminder before commits
#   3. Prefer jj over git

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FLAG_FILE="$SCRIPT_DIR/../.clojure-ready"

input=$(cat)
tool_name=$(echo "$input" | jq -r '.tool_name // empty' 2>/dev/null || true)

# --- Skill: track /clojure invocation ---
if [[ "$tool_name" == "Skill" ]]; then
    skill=$(echo "$input" | jq -r '.tool_input.skill // empty' 2>/dev/null || true)
    [[ "$skill" == "clojure" ]] && touch "$FLAG_FILE"
    exit 0
fi

# --- Bash: prefer jj over git, check commits ---
if [[ "$tool_name" == "Bash" ]]; then
    cmd=$(echo "$input" | jq -r '.tool_input.command // empty' 2>/dev/null || true)
    # Prefer jj over git for common commands
    if [[ "$cmd" =~ ^git\ (status|diff|log|show|add|checkout|branch) ]]; then
        cat << 'EOF'
{"hookSpecificOutput":{"hookEventName":"PreToolUse","additionalContext":"Prefer jj over git: use jj status, jj diff, jj log, jj show, jj new, jj edit instead."}}
EOF
    # Review reminder before commits
    elif [[ "$cmd" =~ ^(jj|git)\ (commit|describe) ]]; then
        cat << 'EOF'
{"hookSpecificOutput":{"hookEventName":"PreToolUse","additionalContext":"Before committing: 1) Run tests (bb test) 2) Review changes (jj diff / jj status)"}}
EOF
    fi
    exit 0
fi

# --- Read: encourage /clojure skill for exploration ---
if [[ "$tool_name" == "Read" ]]; then
    file_path=$(echo "$input" | jq -r '.tool_input.file_path // empty' 2>/dev/null || true)
    [[ "$file_path" =~ \.(clj|cljc|cljs)$ || "$file_path" =~ deps\.edn$ ]] || exit 0
    [[ -f "$FLAG_FILE" ]] && exit 0
    cat << 'EOF'
{"hookSpecificOutput":{"hookEventName":"PreToolUse","additionalContext":"Consider invoking /clojure skill first. It sets up nREPL for interactive exploration - prefer REPL introspection over grep/file reading for Clojure code."}}
EOF
    exit 0
fi

# --- Edit/Write: check Clojure skill ---
[[ "$tool_name" == "Edit" || "$tool_name" == "Write" ]] || exit 0

file_path=$(echo "$input" | jq -r '.tool_input.file_path // empty' 2>/dev/null || true)
[[ "$file_path" =~ \.(clj|cljc|cljs)$ || "$file_path" =~ deps\.edn$ ]] || exit 0
[[ -f "$FLAG_FILE" ]] && exit 0

cat << 'EOF'
{"hookSpecificOutput":{"hookEventName":"PreToolUse","additionalContext":"Invoke /clojure skill before editing Clojure files. It loads project-specific patterns and REPL setup."}}
EOF
