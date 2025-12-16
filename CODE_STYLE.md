# Clojure Code Style Guide
Purpose
This guide standardizes code style and workflow in this repository for both humans and AI agents.

## Scope and tooling
- Language: Clojure 1.12
- Build: Clojure CLI with `deps.edn`, source under `src/`
- Lint: clj-kondo (custom hook for `sg.flybot.pullable.util/cond-let`)
- Tests: rich-comment-tests; regression via `clojure -X:dev:test`
- Tasks: Babashka (`bb.edn`)
- Evaluation: use the `clojure-mcp` MCP server during development

## Project structure and namespaces
- All code lives under `src/sg/flybot/pullable/...`
- Public API: `sg.flybot.pullable`
- Utilities and helpers: `sg.flybot.pullable.util`
- Advanced/experimental features: `sg.flybot.pullable.core2`
- One public concern per namespace; keep files small and cohesive

## Naming
- Vars and functions: kebab-case (e.g., `pull-pattern`)
- Predicates end with `?` (e.g., `valid-pattern?`)
- Side-effecting fns end with `!` (e.g., `write-result!`)
- Private helpers marked with `^:private` or `defn-` as appropriate

## Conditionals and control flow
- Single check: `if`; multiple branches: `cond`
- Bind and test in one step: `when-let` / `if-let`
- Prefer `cond->` and `cond->>` for progressive transformations
- Use threading (`->`, `->>`) to reduce nesting and improve readability
- `cond-let` (project macro): use when you need conditional bindings that flow through branches; keep bindings minimal and local

## Data and errors
- Prefer maps with namespaced keys for domain data
- Return `nil` for “not found”; avoid boolean flags
- Use `ex-info` with data maps for exceptional conditions
- Functions should be pure; isolate effects at the edges

## Collections and performance
- Choose persistent vector/map/set by access pattern; avoid premature optimization
- Prefer sequence functions and transducers for streaming transforms when needed

## Docstrings and examples
- Every public var has a concise docstring
- Include minimal REPL examples in docstrings when non-obvious

## Testing (rich-comment-tests)
- Place tests near the code using `^:rct/test` comment blocks
- Use:
  - `;=>` for value equality
  - `;=>>` for predicate/matcho-style expectations
  - `throws=>>` for exception expectations
- Commands:
  - Regression: `clojure -X:dev:test`
  - Optional shortcut: `bb test`

## Evaluation and MCP
- Always evaluate and inspect via the `clojure-mcp` MCP server during development
- Prefer small, incremental edits verified in REPL before larger changes

## Linting
- Run clj-kondo locally; ensure no new warnings
- The `cond-let` macro is recognized via `.clj-kondo/config.edn` and `hooks/cond_let.clj`; keep usage consistent with the hook

## Git and PR workflow
- Small, focused commits in imperative, present tense
- Keep diffs minimal and self-explanatory; prefer one responsibility per PR
- Do not commit directly from the agent unless explicitly instructed; open a PR

## Command hygiene
- Explain non-trivial commands in commit messages/PR descriptions
- Avoid pagers (e.g., `git --no-pager`); never print secrets; use env vars instead
