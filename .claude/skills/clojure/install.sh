#!/usr/bin/env bash
# Install Clojure skill with hooks into a Claude Code project
#
# Usage:
#   ./install.sh                    # Install to current directory
#   ./install.sh /path/to/project   # Install to specific project
#
# This installs:
#   - Clojure coding guidelines skill
#   - Hook to enforce /clojure before editing Clojure files
#
# For jj (Jujutsu) enforcement, install the jj skill separately.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TARGET_DIR="${1:-.}"
TARGET_DIR="$(cd "$TARGET_DIR" && pwd)"

CLAUDE_DIR="$TARGET_DIR/.claude"
HOOKS_DIR="$CLAUDE_DIR/hooks"
SKILLS_DIR="$CLAUDE_DIR/skills"
SETTINGS_FILE="$CLAUDE_DIR/settings.json"

echo "Installing Clojure skill to: $TARGET_DIR"

mkdir -p "$HOOKS_DIR"
mkdir -p "$SKILLS_DIR/clojure"

echo "  Copying skill..."
cp "$SCRIPT_DIR/SKILL.md" "$SKILLS_DIR/clojure/"
if [[ -d "$SCRIPT_DIR/clojurescript" ]]; then
    cp -r "$SCRIPT_DIR/clojurescript" "$SKILLS_DIR/clojure/"
fi

echo "  Copying hooks..."
cp "$SCRIPT_DIR/hooks/clojure-guardian.sh" "$HOOKS_DIR/"
chmod +x "$HOOKS_DIR/clojure-guardian.sh"

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

GITIGNORE="$TARGET_DIR/.gitignore"
if [[ -f "$GITIGNORE" ]]; then
    if ! grep -q ".claude/settings.local.json" "$GITIGNORE" 2>/dev/null; then
        echo ".claude/settings.local.json" >> "$GITIGNORE"
    fi
fi

echo ""
echo "Done! Clojure skill installed."
echo ""
echo "Features:"
echo "  - /clojure skill with coding guidelines"
echo "  - Edit/Write blocked until /clojure is invoked"
echo ""
echo "To also enforce jj over git, install the jj skill:"
echo "  ../jj/install.sh $TARGET_DIR"
