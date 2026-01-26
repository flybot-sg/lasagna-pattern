# Clojure Skill for Claude Code

Clojure development guidelines with enforcement hooks.

## Features

- **REPL-first workflow** - Encourages nREPL exploration over grep
- **Coding guidelines** - Data-oriented design, pure functions, idiomatic patterns
- **Post-task review** - Reminds to simplify code after changes
- **Skill gate** - Blocks Clojure file edits until `/clojure` is invoked
- **Self-improving** - Agent updates skill when discovering new patterns

## Installation

```bash
./install.sh /path/to/your/project
```

## What Gets Installed

```
.claude/
├── settings.json              # Hook configuration
├── hooks/
│   └── clojure-guardian.sh    # Skill enforcement
└── skills/
    └── clojure/
        ├── SKILL.md           # Main skill
        └── clojurescript/     # ClojureScript subskill
```

## Usage

```
/clojure    # Load skill (required before editing .clj files)
```

## Related Skills

For jj (Jujutsu) version control enforcement, install separately:
```bash
../jj/install.sh /path/to/your/project
```

## Customization

Edit `.claude/hooks/clojure-guardian.sh` to change which files require the skill.
