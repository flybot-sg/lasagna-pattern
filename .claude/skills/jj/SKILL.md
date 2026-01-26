---
name: jj
description: Jujutsu (jj) version control workflow. Use when the project uses jj instead of git. Provides command equivalents and workflow patterns.
---

# Jujutsu (jj)

This project uses **jj** (Jujutsu) for version control, not git.

## Command Reference

| Task | jj command | git equivalent |
|------|------------|----------------|
| Status | `jj status` or `jj st` | `git status` |
| Diff | `jj diff` | `git diff` |
| Log | `jj log` | `git log` |
| Show commit | `jj show` | `git show` |
| Create commit | `jj new` | `git commit` |
| Set commit message | `jj describe -m "msg"` | `git commit -m "msg"` |
| Amend | `jj squash` | `git commit --amend` |
| Switch commits | `jj edit <rev>` | `git checkout` |
| Branches | `jj bookmark list` | `git branch` |
| Create branch | `jj bookmark create <name>` | `git branch <name>` |
| Push | `jj git push` | `git push` |
| Fetch | `jj git fetch` | `git fetch` |
| Undo | `jj undo` | `git reset` |
| Abandon commit | `jj abandon` | - |
| List workspaces | `jj workspace list` | `git worktree list` |
| Add workspace | `jj workspace add <path>` | `git worktree add` |
| Remove workspace | `jj workspace forget <name>` | `git worktree remove` |

## Key Differences

### No Staging Area
jj has no staging area. All changes in the working copy are automatically part of the current commit.

```bash
# Just describe and create new commit
jj describe -m "Add feature"
jj new
```

### Working Copy is a Commit
The working copy is always a commit (the `@` commit). Changes are tracked automatically.

### Conflicts are First-Class
Conflicts can be committed and resolved later. No need to resolve immediately.

## Workspaces (Parallel Tasks)

**Use workspaces to work on multiple tasks simultaneously** without stashing or switching branches.

```bash
# List workspaces
jj workspace list

# Create workspace for a new task
jj workspace add ../project-feature-x  # Creates sibling directory
cd ../project-feature-x

# Or create at specific commit
jj workspace add ../project-bugfix --revision @-

# Delete workspace when done
jj workspace forget feature-x
```

**Each workspace:**
- Has its own working copy
- Shares the same repo/history
- Can be at different commits
- Changes are visible across workspaces after commit

**Workflow for parallel tasks:**
```bash
# Main workspace: working on feature A
cd ~/project

# Need to fix urgent bug? Create new workspace
jj workspace add ../project-hotfix --revision main
cd ../project-hotfix
# Fix bug here...
jj describe -m "Fix critical bug"
jj git push

# Back to feature A
cd ~/project
# Continue working...
```

## Common Workflows

### Make Changes
```bash
# Edit files...
jj status          # See changes
jj diff            # Review changes
jj describe -m "Description of changes"
jj new             # Start new commit
```

### Fix Previous Commit
```bash
jj edit <rev>      # Go back to commit
# Make fixes...
jj squash          # Fold into parent, or
jj new             # Create new commit on top
```

### Push to Remote
```bash
jj bookmark create my-branch  # Create bookmark if needed
jj git push --bookmark my-branch
```

## Self-Improvement

**Update this skill when you discover useful jj patterns.**

**Location:** `$CLAUDE_PROJECT_DIR/.claude/skills/jj/SKILL.md`

## Learned Patterns

<!-- Add new discoveries below this line -->
