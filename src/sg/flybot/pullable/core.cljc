(ns sg.flybot.pullable.core
  "Core design, data structure and functions.
   Keep it pure functional."
  (:require [clojure.walk :as walk]))

;;# Matcher and matcher result
;;
;; A matcher is a function takes a match result (mr) and returns a mr, or nil if not matching.
;;
;; A matcher result is a map contains `:val` as it value, `:vars` is a map contains
;; matching variable values.

(defn matcher
  "Attach necessary metadata to matcher function `f` with matcher type `t`,
   can optional provide `more` metadata in k-v pairs"
  [f t & {:as more}] (with-meta f (merge more {::matcher-type t})))

(defn matcher-type
  "returns the matcher type of `x`, if not a matcher, returns false"
  [x]
  (and (fn? x) (some-> x meta ::matcher-type)))

;;## Implementation of matchers 

(defn mpred
  "a basic matcher returns the mr itself when `pred` success, its type is `:pred`"
  [pred]
  (matcher
   (fn [mr]
     (when (pred (:val mr))
       mr))
   :pred))

(defn mval
  "a pred matcher matchers when mr's value equals v"
  [v] (mpred #(= v %)))

(def wildcard
  "a pred matcher always success"
  (mpred (constantly true)))

(defn mvar
  "a matcher binds `child` matchers result to a symbol `sym` when `child` success,
   it also checks if the new result equals to known value of the variable, if not,
   the match fails.

  `child` matcher can specify a `:captured` value in mr, this matcher will use it
   instead of `:val`."
  [sym child]
  (matcher
   (fn [mr]
     (when-let [mr (child mr)]
       (let [new-v (or (:captured mr) (:val mr))
             old-v (get-in mr [:vars sym] ::not-found)
             mr (dissoc mr :captured)]
         (condp = old-v
           ::not-found (assoc-in mr [:vars sym] new-v) ;a fresh match
           new-v mr ;current value agrees with existing value
           nil))))
   (matcher-type child) ::symbol sym))

^:rct/test
(comment
  ;;mpred succeed when `pred` success
  ((mpred even?) {:val 4}) ;=>> {:val 4}
  ;;mpred fails
  ((mpred even?) {:val 3}) ;=> nil
  ;;wildcard example
  (wildcard {:val 3}) ;=>> {:val 3}

  ;;mvar bind a fresh variable 'a
  ((mvar 'a (mval 3)) {:val 3}) ;=>
  {:val 3 :vars {'a 3}}
  ;;mvar unification
  ((mvar 'a (mval 3)) {:val 3 :vars {'a 3}}) ;=>>
  {:val 3}
  ;;mvar unification fails
  ((mvar 'a (mval 3)) {:val 3 :vars {'a 2}}) ;=>
  nil
  )

;;## Map matcher

(defn mmap
  "Create a map matcher from a collection of `[k matcher]` pairs.
   Looks up each key in the current `:val` map, runs its matcher and
   accumulates both the transformed map and merged variable bindings."
  [k-matchers]
  (matcher
   (fn [mr]
     (reduce
      (fn [mr' [k mch]]
        (if-let [vmr (some-> (update mr' :val get k) (mch))]
          (-> mr'
                (update :val conj [k (:val vmr)])
                (update :vars merge (:vars vmr)))
          (reduced nil)))
      mr k-matchers))
   :map))

^:rct/test
(comment
  (def mm (mmap [[:a (mvar 'a (mval 4))] [:b (mpred even?)]]))
  (mm {:val {:a 4 :b 0}})  ;=>
  {:val {:a 4 :b 0} :vars {'a 4}}
  (mm {:val {:a 4 :b 3}}) ;=> nil
  (mm {:val {:a 4 :b 0} :vars {'a 0}})) ;=> nil

;;## Sequence matchers

(def mnone
  "A subsequence matcher that matches an empty subsequence only.
   Used as the `:none` branch in higher level sequence combinators."
  (matcher
    identity
    :subseq ::length [0 0]))

(defn mone
  "Subsequence matcher that consumes exactly one element from `:rest`
   and passes it to `child`. Propagates `:vars` and updates `:rest`."
  [child]
  (matcher
   (fn [mr]
     (when-let [mr' (some->> (:rest mr) first (assoc mr :val) child)]
       (some-> mr (update :vars merge (:vars mr')) (update :rest rest))))
   :subseq ::length [1 1]))

(defn mrest
  "Subsequence matcher that repeatedly applies `child` to elements in
   `:rest`, capturing all consecutive successes as a vector in
   `:captured` and dropping them from `:rest`."
  [child]
  (matcher
   (fn [mr]
     (let [melems (->> (:rest mr) (map #(child (assoc mr :val %))) (take-while identity))]
       (-> mr
           (assoc :rest (->> (:rest mr) (drop (count melems))))
           (assoc :captured (->> melems (map :val) (reduce conj []))))))
   :subseq ::length [0]))

(defn msubseq
  "Compose a sequence of subsequence matchers.
   Applies each matcher to the same matcher result in order until one
   fails, returning the last successful matcher result or `nil`."
  [& matchers]
  (matcher
   (fn [mr]
     (reduce (fn [mr' mch] (or (mch mr') (reduced nil))) mr matchers))
   :subseq))

(defn mseq
  "Top level sequence matcher.
   Treats the current `:val` as the full sequence, stores it in `:rest`,
   runs `matchers` left-to-right and succeeds only when the whole
   sequence is consumed."
  [matchers]
  (matcher
   (fn [mr]
     (let [mr (reduce (fn [mr' mch]
                        (or
                         (mch mr')
                         (reduced nil)))
                      (assoc mr :rest (:val mr))
                      matchers)]
       (when-not (some-> mr :rest seq)
         (dissoc mr :rest))))
   :seq))

^:rct/test
(comment
  ;mone matches a single sequence value
  (def mo (mone (mvar 'a (mval 1))))
  (mo {:val [1] :rest [1]}) ;=>> {:vars {'a 1} :rest []}
  (mo {:val [2] :rest [2]}) ;=> nil
  (mo {:val [] :rest []}) ;=> nil

  (def mr (mvar 'a (mrest (mpred even?))))
  (mr {:val [2 4 6 1] :rest [2 4 6 1]}) ;=>> {:vars {'a [2 4 6]} :rest [1]}
  (mr {:val [2 0] :rest [2 0] :vars {'b -1}})

  ;mseq matches a whole sequence
  (def sm (mseq [mo (mone (mpred zero?)) (mone (mpred odd?))]))
  (sm {:val [1 0 3]}) ;=>> {:vars {'a 1}}
  (sm {:val [1 0 3 2]}) ;=> nil

  (def sm (mseq [(mone (mvar 'b (mval 1))) (mone (mpred zero?)) (mvar 'a (mrest (mpred odd?)))]))
  (sm {:val [1 0 1 3 5]}) ;=>> {:vars {'a [1 3 5]}}
  (sm {:val [1 0 1 3 5 2]}) ;=> nil

  (def sm2 (mseq [(mone (mvar 'b (mval 1))) (msubseq (mone (mpred zero?)))]))
  (sm2 {:val [1 0]}))

;;## Misc matchers

(defn msub
  "Value transforming matcher.
   Runs `child` first and then replaces `:val` with `(f (:val mr))` while
   keeping the rest of the matcher result intact."
  [f child]
  (matcher
   (fn [mr]
     (when-let [mr (child mr)]
       (let [v (f (:val mr))]
         (assoc mr :val v))))
   :sub))

(def find-first (comp first filter))

(defn mor
  "Branching matcher that tries alternatives in order.
   `kms` is a flat collection `[k1 m1 k2 m2 ...]`; the first successful
   matcher wins and its branch key is stored in `:captured`."
  [kms]
  (let [km-pair (partition 2 kms)]
    (matcher
     (fn [mr]
       (when-let [[t mr'] (->> km-pair
                               (map (fn [[k mch]] [k (mch mr)]))
                               (find-first second))]
         (assoc mr' :captured t)))
     :or)))

(defn mwalk
  "Create a structural matcher based on `clojure.walk/postwalk`.
   `pms` is a flat collection of `pred matcher` pairs used to rewrite
   subforms; `final-f` is applied to the fully rewritten value."
  [final-f & pms]
  (let [pm-pair (partition 2 pms)]
    (matcher
     (fn [mr]
       (update mr :val
               (fn [v]
                 (final-f
                  (walk/postwalk
                   (fn [x]
                     (or
                      (->> pm-pair
                           (map (fn [[pred mch]] (when (pred x) (:val (mch (assoc mr :val x))))))
                           (find-first identity))
                      x))
                   v)))))
     :walk)))

(defn mmulti
  "Higher order matcher that looks up variable `sym` in `:vars`, calls
   `(f v)` to obtain a matcher and then runs it against the current
   matcher result."
  [f sym]
  (matcher
   (fn [mr]
     (let [v (get-in mr [:vars sym])]
       ((f v) mr)))
   :multi))

(defn moption
  "Optional subsequence matcher.
   Either consumes `child` once (`:one`) or nothing (`:none`) and then
   continues with `more`. The chosen branch type is captured in
   `:captured`."
  ([child]
   (moption child wildcard))
  ([child more]
   (mor [:one (msubseq (mone child) more)
         :none (msubseq mnone more)])))

^:rct/test
(comment
  ;msub substitute :val and :captured
  (def ms (msub inc (mval 1)))
  (ms {:val 1}) ;=>> {:val 2}

  ;mor find the first matched branch of patterns and capture the type
  (def mo (mvar 'type (mor [:keyword (mpred keyword?)
                            :number (msub str (mpred number?))])))
  (mo {:val 3}) ;=>> {:vars {'type :number}}

  ((moption (mvar 'a wildcard) wildcard) {:rest []}) ;=>> {:captured :none}
  ((moption (mvar 'a wildcard) wildcard) {:val [1] :rest [1]}) ;=>> {:captured :one}
  (map (mseq [(moption (mvar 'a (mval 0)) (mone (mvar 'b (mval 1))))]) [{:val [1]} {:val [0 1]}]) ;=>>
  [{:vars {'b 1}} {:vars {'a 0 'b 1}}]

  (def mw (mwalk
           identity
           list? (msub #(apply * %) (mseq [(mone wildcard) (mone (mpred number?))]))
           number? (msub #(* 2 %) (mpred number?))))
  (mw {:val '(2 5)}) ;=>> {:val 40}
  (mw {:val '(4 (2 5))}) ;=>>
  {:val 320}
  )

;;## Pattern
;;
;; Pattern is a mini language using common clojure data, it can translate to matcher functions.

(defmulti make-matcher
  "Compile a low level pattern description like `[:pred pred]` or
   `[:seq ...]` into a concrete matcher function."
  first)
(defmethod make-matcher :pred [[_ pred]] (mpred pred))
(defmethod make-matcher :val [[_ v]] (mval v))
(defmethod make-matcher :var [[_ sym child]] (mvar sym child))
(defn wrap-element
  "Ensure a pattern element behaves like a subsequence matcher.
   Plain values become `mval` matchers and non-subseq matchers are
   wrapped with `mone` so they consume exactly one element."
  ([item]
   (let [mt (matcher-type item)] (cond-> item (false? mt) mval (not= mt :subseq) mone))))

(defmethod make-matcher :seq [[_ & children]]
  (mseq (map wrap-element children)))
(defmethod make-matcher :map [[_ & kvs]]
  (mmap (map (fn [[k mch]] [k (cond-> mch (not (matcher-type mch)) mval)]) (partition 2 kvs))))
(defmethod make-matcher :sub [[_ f child]] (msub f child))
(defmethod make-matcher :or [[_ & conditions]] (mor conditions))
(defmethod make-matcher :rest [[_ child]] (mrest (or child wildcard)))
(defmethod make-matcher :? [[_ child more]] (moption child more))

(defmulti matcher-args
  "Describe how many and what kinds of arguments a matcher form expects.
   The dispatch value is the matcher type keyword, e.g. `:seq` or
   `:pred`."
  identity)
(defmethod matcher-args :default [_] (mrest wildcard))
(defmethod matcher-args :seq [_] (mrest (mpred matcher-type)))
(defmethod matcher-args :pred [_] (mone (mpred ifn?)))
(defmethod matcher-args :val [_] (mone wildcard))
(defmethod matcher-args :var [_] (msubseq (mone (mpred symbol?)) (mone (mpred matcher-type))))
(defmethod matcher-args :sub [_] (msubseq (mone (mpred ifn?)) (mone (mpred matcher-type))))

(defn named-var?
  "Parse a symbol as a named pattern variable.
   Returns `[base-name multi? optional?]` when `v` has the `?x`, `??x` or
   `?x?` forms, otherwise `nil`."
  [v]
  (when (symbol? v)
    (when-let [[_ s n ?] (re-matches #"(\?[\?]?)([a-zA-Z][a-zA-Z0-9_-]*)(\??)" (name v))]
      [(symbol n) (= s "??") (= ? "?")])))

^:rct/test
(comment
  (named-var? '?x3) ;=>> ['x3 false]
  (named-var? '??x?) ;=>>
  ['x true])

(defn ptn->fn
  "compiles a ptn syntax to a function matches any data, returns a matching result"
  [ptn]
  (let [var? #{'? '??}
        mf (mwalk
            ;;if returns plan value, make it a val matcher
            (fn [v] (if (matcher-type v) v (mval v)))

            ;; support general matcher syntax
            (fn [v] (and (list? v) (var? (first v))))
            (msub (fn [v] (cond-> (make-matcher (rest v)) (= '?? (first v)) mrest))
                  (mseq [(mone (mpred var?)) (mone (mvar 'type (mpred keyword?))) (mmulti matcher-args 'type)]))

            ;; map is a shortcut for :map matcher
            map?
            (msub (fn [v] (make-matcher (cons :map (flatten (vec v))))) wildcard)

            ;; vector is a shortcut for :seq matcher
            (fn [x] (and (vector? x) (not (map-entry? x))))
            (msub (fn [v] (make-matcher (cons :seq v))) wildcard)

            ;; wildcard is ?_
            #(= % '?_)
            (msub (constantly wildcard) wildcard)

            ;; named var is a shortcut for var wildcard
            named-var?
            (msub (fn [v] (let [[var-name s? optional?] (named-var? v)]
                           (cond-> (mvar var-name (cond-> wildcard s? mrest))
                             optional? (moption))))
                  wildcard))
        rslt (mf {:val ptn})]
    (fn [data]
      (when rslt ((:val rslt) {:val data})))))

^:rct/test
(comment
  ((ptn->fn '(? :var a (? :val 20))) 20)                        ;=>> {:val 20}
  ((ptn->fn '[(? :var a (?? :val 20))]) [20 20]) ;=>> {:vars {'a [20 20]}}
  ((ptn->fn '(? :seq (? :var a (? :val 20)))) [20]) ;=>> {:val [20] :vars {'a 20}}
  ((ptn->fn '(? :map :a (? :var a (? :val 20)))) {:a 20}) ;=>> {:val {:a 20} :vars {'a 20}}
  ((ptn->fn '?_) 23)             ;=>> {:val 23}
  ((ptn->fn '[?_ ?_]) [2 1])     ;=>> {:val [2 1]}
  ((ptn->fn '[?a ?b]) [3 4])     ;=>> {:vars '{a 3 b 4}}
  ((ptn->fn '{:a ?a :b 3}) {:a 5 :b 3})    ;=>> {:vars '{a 5}}
  ((ptn->fn '[5 ?a]) [5 -1])     ;=>> {:vars '{a -1}}
  ((ptn->fn 5) 5) ;=> {:val 5}
  ((ptn->fn (list '? :sub #(* % %) (list '? :pred number?))) 8) ;=> {:val 64}
  ((ptn->fn (list '? :pred odd?)) 3) ;=> {:val 3}
  ((ptn->fn (list '? :var 'type (list '? :or :number (list '? :pred number?) :string (list '? :pred string?)))) 3) ;=>>
  {:val 3 :vars {'type :number}}
  ((mseq [(mone (mval 1)) (mone (mval 2)) (mone (mval 3)) (mvar 'a (mrest wildcard))]) {:val [1 2 3 4 5]})
  ((ptn->fn '[1 2 3 ??end]) [1 2 3 4 5]) ;=>> {:vars {'end [4 5]}}

  ;;optional pattern
  ;;(map (ptn->fn '[?a? 2]) [[0 2] [2]]) ;=>>
  ;; [{:vars {'a 1}} {}]

  ;; complex pattern unification
  (mapv (ptn->fn '{:a ?x :b {:c ?x}}) [{:a 3 :b {:c 3}} {:a 2 :b {:c 3}}]) ;=>>
  [{:vars {'x 3}} nil]
  (mapv (ptn->fn '[?x 5 ?_ {:a ?x}]) [[3 5 6 {:a 3}] [3 1]]) ;=>>
  [{:vars {'x 3}} nil])
