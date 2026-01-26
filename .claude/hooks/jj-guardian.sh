#!/usr/bin/env bash
# jj guardian - enforces Jujutsu over git
#
# Rules:
#   - Block all git commands
#   - Reminder before jj commits to run tests

set -euo pipefail

input=$(cat)
tool_name=$(echo "$input" | jq -r '.tool_name // empty' 2>/dev/null || true)

[[ "$tool_name" == "Bash" ]] || exit 0

cmd=$(echo "$input" | jq -r '.tool_input.command // empty' 2>/dev/null || true)

# Block git commands
if [[ "$cmd" =~ ^git\  ]]; then
    cat << 'EOF'
{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"Use jj, not git. Equivalents: jj status, jj diff, jj log, jj show, jj new (commit), jj describe -m (msg), jj git push"}}
EOF
    exit 0
fi

# Reminder before jj commits
if [[ "$cmd" =~ ^jj\ (describe|new|commit) ]]; then
    cat << 'EOF'
{"hookSpecificOutput":{"hookEventName":"PreToolUse","additionalContext":"Before committing: 1) Run tests 2) Review changes (jj diff / jj status)"}}
EOF
fi
