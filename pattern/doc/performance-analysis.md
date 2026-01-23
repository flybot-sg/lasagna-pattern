# Performance Analysis: flybot.pullable

This document analyzes the performance characteristics of the `flybot.pullable` pattern matching library across different usage scenarios.

## 1. Compilation vs. Runtime Trade-off

| Scenario | Recommendation |
|----------|----------------|
| Single use pattern | Use raw patterns (`query '{:a ?a} data`) |
| Repeated matching | Pre-compile with `(compile pattern)` |

**Compilation cost** (`ptn->matcher`):
- Uses `walk/prewalk` + `walk/postwalk` - O(n) where n = pattern size
- Pattern parsing involves regex matching for `?x`, `?x+`, etc.
- Creates closures for each matcher primitive

**Runtime matching** (compiled matcher):
- Direct function application - very fast
- No pattern parsing overhead

## 2. Pattern Complexity Scaling

| Pattern Type | Time Complexity | Notes |
|--------------|-----------------|-------|
| `mval` (exact match) | O(1) | Simple equality check |
| `mpred` (predicate) | O(1) + pred cost | Pred function dominates |
| `mmap` (map match) | O(k) | k = pattern keys (uses `reduce`) |
| `mseq` (sequence) | O(n) | n = elements, uses zipper |
| `mor` (alternatives) | O(m) worst | m = alternatives, short-circuits |
| `mzrepeat` (quantified) | O(n × r) | r = range size, backtracking |

## 3. Sequence Matching Performance

The library uses `clojure.zip` for sequence operations:

```clojure
;; core.cljc:403-404 - zipper creation per match
(vmr (-> original seq zip/seq-zip zip/next) (:vars mr))
```

**Performance characteristics:**

| Operation | Cost |
|-----------|------|
| Zipper creation | O(1) - lazy |
| `zip/next` traversal | O(1) per step |
| `zip/replace` + rebuild | O(log n) amortized |
| Full sequence traversal | O(n) |

**Bottleneck scenarios:**

- **Large sequences with transformations**: `zip/replace` creates new zipper nodes
- **Deep nesting**: Each nested structure creates a new zipper

## 4. Backtracking Patterns (Costly Scenarios)

### 4a. Lazy vs. Greedy Quantifiers

```clojure
;; Lazy (default) - tries minimum first
'[?a* ?b*]   ;; O(n) - finds solution fast

;; Greedy - tries maximum first
'[?a*! ?b*]  ;; O(n) - finds solution fast (greedy direction)

;; Mixed with constraints - backtracking required
'[?a+ ?b+ ?c+]  ;; O(n²) worst case - must try different splits
```

**Implementation** (`try-lengths` in core.cljc:476-487):
```clojure
(loop [[len & more] lengths]
  (if (nil? len)
    (fail ...)
    (let [attempt (nxt-matcher (assoc zmr :val z-at-len))]
      (if (failure? attempt)
        (recur more)  ;; Backtrack to next length
        ...))))
```

### 4b. `mor` (OR) Backtracking

```clojure
;; core.cljc:220-232 - tries each alternative
(loop [[m & rest] matchers best-fail nil]
  (let [result (m mr)]
    (if (failure? result)
      (recur rest (deeper-failure best-fail result))  ;; Try next
      result)))  ;; Short-circuit on success
```

**Cost**: O(m) where m = alternatives. First match wins, so order matters.

## 5. Variable Binding Performance

```clojure
;; core.cljc:97-101 - binding with conflict detection
(-bind [this sym v]
  (let [old-v (get vars sym ::not-found)]
    (condp = old-v
      ::not-found (assoc vars sym v)  ;; O(log₃₂ n)
      v this                           ;; O(1) - same value
      (fail ...))))                    ;; Conflict
```

- Uses persistent hash maps - O(log₃₂ n) ≈ O(1) for binding
- **Variable unification** (same var appearing multiple times) adds equality checks

## 6. Specific Scenario Performance

| Scenario | Complexity | Example |
|----------|------------|---------|
| **Simple map extraction** | O(k) | `'{:a ?a :b ?b}` |
| **Nested map** | O(k₁ × k₂) | `'{:a {:b ?x}}` |
| **Fixed-length sequence** | O(n) | `'[?a ?b ?c]` |
| **Head + rest** | O(n) | `'[?head ?tail+]` |
| **Multiple quantifiers** | O(n²) | `'[?a+ ?b+ ?c+]` |
| **Alternatives in sequence** | O(m × n) | `'[(? :or ...) ...]` |
| **Filter over sequence** | O(n) | `'(? :filter even? 'evens)` |
| **Find first** | O(n) worst | `'(? :first pred)` |

## 7. Memory Usage Patterns

| Component | Memory Behavior |
|-----------|-----------------|
| Compiled matchers | Closures with captured rules - fixed overhead |
| `ValMatchResult` | Small record (val + vars map) |
| `MatchFailure` | Contains path vector - grows with nesting depth |
| Zipper state | Shares structure with original - efficient |
| Variable bindings | Persistent map - structural sharing |

## 8. Performance Recommendations

### Best Practices

1. **Pre-compile patterns** for repeated use:
   ```clojure
   (def my-matcher (p/compile pattern))  ;; Once
   (p/query my-matcher data)             ;; Many times
   ```

2. **Order alternatives by likelihood**: `mor` short-circuits on first success

3. **Prefer specific patterns over backtracking**:
   ```clojure
   ;; Slower - requires backtracking
   '[?prefix* :marker ?suffix*]

   ;; Faster - no backtracking if you know structure
   '[(? :filter #(not= :marker %) 'prefix) :marker ?suffix*]
   ```

4. **Use predicates for early failure**: Fail fast rather than match-then-check

5. **Avoid deeply nested quantifiers**:
   ```clojure
   ;; O(n³) potential
   '[[?a*]* ?b*]
   ```

## 9. Worst-Case Patterns to Avoid

```clojure
;; Exponential backtracking potential
'[?a+ ?b+ ?c+ ?d+]  ;; O(n³) for n elements

;; Nested quantifiers on large sequences
'[[?inner*]* ?outer*]

;; Many alternatives with similar prefixes
(? :or [1 2 ?a ...] [1 2 ?b ...] [1 2 ?c ...])
```

## 10. Summary

| Scenario | Performance | Notes |
|----------|-------------|-------|
| Map matching | Excellent | Linear in pattern keys |
| Fixed sequences | Excellent | Linear traversal |
| Single quantifier | Good | Linear with zipper overhead |
| Multiple quantifiers | Fair | Backtracking adds O(n) factor |
| Deep nesting | Fair | Zipper creation per level |
| Complex alternatives | Variable | Depends on match order |

The library is well-suited for structured data extraction patterns typical in API responses and configuration parsing. For extremely large sequences or patterns requiring extensive backtracking, consider chunking data or restructuring patterns.
