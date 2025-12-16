# Agent Guide
Purpose
This document defines how we use an AI agent to work on this repository: expectations, coding/testing conventions, and review workflow.

## How we use the agent
- Prefer small, incremental changes with clear diffs.
- Always explain why a non-trivial command or edit is being made.
- Never commit directly; open a PR unless explicitly instructed otherwise.
- Keep responses concise and objective; avoid filler text.

## MCP and dev workflow
- Always use the `clojure-mcp` MCP server during development and for code evaluation.
- Run regression tests with `clojure -X:dev:test`.

## Working agreement
- Treat the repository’s linters, tests, and CI as the source of truth.
- When information could be out-of-date, verify before asserting.
- Use absolute dates (e.g., 2025-12-16) when clarifying timelines.

## Files and paths in conversations
- Reference files using repository-relative paths, e.g., `src/foo.clj`, `README.md`.
- Use absolute paths only for system-level files.

## Code edits
- Adhere to existing patterns in the codebase.
- Prefer pure, small functions; one responsibility per function.
- Minimize nesting; use early returns.
- Threading macros (`->`, `->>`) when they improve clarity.
- Keep predicates named with `?` suffix.
- Avoid comments unless clarifying something non-obvious.

## Testing
- Aim for isolated unit tests; don’t rely on other project functions unless necessary.
- If using Clojure, prefer rich-comment-tests over `clojure.test`.
  - Example rich-comment-test form:
    ```clojure
    ^:rct/test
    (comment
      (+ 5 3) ;=>
      8
      (+ 5 9) ;=>>
      even?
      (throw (ex-info "example" {:a "ok"})) ;throws=>>
      #:error{:data {:a "ok"}})
    ```
- Test both success paths and error conditions.

## Shell commands
- Explain what a command does and why it’s run.
- Avoid pagers (e.g., use `--no-pager` with `git`).
- Never echo or print secrets. Load secrets into environment variables and pass references, not values.
- Prefer repo scripts and standard package scripts over ad-hoc commands.

## Jujutsu version management
- Small, focused commits with imperative, present-tense messages.

## jj commit checklist   
- Lint passes locally.
- Tests added/updated and passing.
- User-facing changes documented in `README.md` or relevant docs.
- Summary of changes explains the “why”, not just the “what”.

## Asking for clarification
- If requirements are ambiguous, ask targeted questions before making large changes.

## When to refuse or defer
- If a task risks data loss or production impact, stop and request confirmation.

## Appendix: Clojure conventions (if applicable)
- Prefer `clojure.string` helpers over Java interop for strings.
- Use `if` for single checks; `cond` for multiple branches; consider `when`, `if-let`, `when-let`, `cond->`, `cond->>`.
- Destructure params when accessing multiple keys.
- Reload namespaces before testing in the REPL: `(require '[ns] :reload)`; switch into the namespace with `in-ns`.
