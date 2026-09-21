# Contributing

Commits follow [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/), with the stylistic rules of [@commitlint/config-conventional](https://github.com/conventional-changelog/commitlint/tree/master/%40commitlint/config-conventional). The changelog follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and versions follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Filing issues

An issue is a **problem statement**. The title says what is wrong, the body shows it, and an optional suggestion proposes a fix.

### Title

Phrase the title as the problem, not the solution.

- Good: `Query params with null values cause 500 errors`
- Bad: `Add null handling to query params`

### Body

    ## Problem

    What is wrong. A minimal reproducing code block, the command that surfaces it, the output, and permalinks to the code.

    ## Suggestion

    Optional. The concrete change as a code block, and why. Drop the section when you have none.

Show, do not describe. A block someone can paste beats a paragraph. Two sections only.

## Labeling issues with affected components

Lasagna Pattern is a monorepo. Every issue carries one or more labels naming the
components it touches, and the label matches the commit scope:

- `comp:<component>` for each of: `pattern`, `collection`, `remote`, `flybot-site`, `pull-playground`

## Pull requests

Pull requests target `main`. Branch from it, named `<issue-number>-<short-description>`, e.g. `16-validate-mutation-input`.

### Title

Same format as a commit title, one line:

    <prefix>(<scope>): <short description>

### Description

    Closes #<issue-number>
    ---

    - First change description
    - Second change description

Keep bullets short; the issue carries the full context.

### Changelog entry

Components version and release independently, so each keeps its own `CHANGELOG.md`. A pull request with a user-facing change adds an entry under `## [Unreleased]` in the changelog of every component it touches, in the same pull request, while the context is fresh. Describe the change as a user experiences it, the old symptom and the new behavior rather than the implementation, and end with the issue link. Internal refactors and test-only changes need no entry.

### Before opening one

Run the local gate first; CI runs the same checks:

```bash
bb test
```

With [`cljr`](https://github.com/clojure/clr.core.cli) installed, also run the ClojureCLR suites (CI does):

```bash
bb cljr-test
```

## Commits

    <prefix>(<scope>): <description>

| Prefix | Use for |
|--------|---------|
| `feat` | New features |
| `fix` | Bug fixes |
| `refactor` | Code restructuring, no behavior change |
| `perf` | Performance improvement |
| `docs` | Documentation only |
| `test` | Adding or updating tests |
| `build` | Build system or dependencies |
| `ci` | CI configuration |
| `style` | Formatting only |
| `revert` | Reverts an earlier commit |
| `chore` | Anything else that touches no source |

- One line, under 100 characters.
- Lowercase description, no trailing period.
- Imperative mood: `add feature`, not `added feature`.
- The scope is required and names the component: `pattern`, `collection`, `remote`, `flybot-site`, `pull-playground`, or `repo` for a repo-wide change.
- Details belong in the PR description, not the commit.

Keep the body to a few plain sentences: the what and the non-obvious why. Do not list the files touched, narrate the steps taken, or report test results; the diff and CI already show those. LLM-generated messages tend to include all three, so trim them before committing.

Reference the issue in the title or body, e.g. `(#42)` or `Closes #42`. Issue references belong in commit messages and PR descriptions only, never in source files or comments: trackers migrate, and in-code numbers go stale.

## LLM context files

LLM context files are not committed. The only exception could be `AGENTS.md`, and we still recommend against it.
