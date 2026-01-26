#!/usr/bin/env bash
# Install jj skill with hooks into a Claude Code project
#
# Usage:
#   ./install.sh                    # Install to current directory
#   ./install.sh /path/to/project   # Install to specific project

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TARGET_DIR="${1:-.}"
TARGET_DIR="$(cd "$TARGET_DIR" && pwd)"

CLAUDE_DIR="$TARGET_DIR/.claude"
HOOKS_DIR="$CLAUDE_DIR/hooks"
SKILLS_DIR="$CLAUDE_DIR/skills"
SETTINGS_FILE="$CLAUDE_DIR/settings.json"

echo "Installing jj skill to: $TARGET_DIR"

mkdir -p "$HOOKS_DIR"
mkdir -p "$SKILLS_DIR/jj"

echo "  Copying skill..."
cp "$SCRIPT_DIR/SKILL.md" "$SKILLS_DIR/jj/"

echo "  Copying hooks..."
cp "$SCRIPT_DIR/hooks/jj-guardian.sh" "$HOOKS_DIR/"
chmod +x "$HOOKS_DIR/jj-guardian.sh"

echo "  Configuring hooks..."
if [[ -f "$SETTINGS_FILE" ]]; then
    cp "$SETTINGS_FILE" "$SETTINGS_FILE.backup"
    if command -v jq &> /dev/null; then
        jq -s '.[0] * .[1]' "$SETTINGS_FILE.backup" "$SCRIPT_DIR/settings.json" > "$SETTINGS_FILE"
        echo "  Merged with existing settings (backup: settings.json.backup)"
    else
        echo "  WARNING: jq not found, please manually merge settings"
    fi
else
    cp "$SCRIPT_DIR/settings.json" "$SETTINGS_FILE"
fi

echo ""
echo "Done! jj enforcement is now active."
echo ""
echo "Features:"
echo "  - git commands blocked (use jj instead)"
echo "  - Commit reminders to run tests first"
echo "  - /jj skill for command reference"
