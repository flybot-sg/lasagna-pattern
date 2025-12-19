;; Design decisions (internal module)
;; - Internal namespace; not a stable public API. Promote select fns via sg.flybot.pullable.
;; - Favor pure transformations on persistent data; namespaced map keys for domain data.
;; - Signal errors with ex-info + data; return nil for "not found" conditions.
;; - Prefer cond-> and threading to reduce nesting; use project cond-let where clearer.
;; - Co-locate ^:rct/test rich-comment-tests near functions.

(ns sg.flybot.pullable.core
  "Internal core of the pullable engine.

   Scope
   - Houses core transformation primitives used by higher-level APIs in sg.flybot.pullable.
   - Internal module; may evolve without stable API. Prefer sg.flybot.pullable for public entry points.

   Conventions
   - Pure, small functions; namespaced map keys.
   - Use ex-info with data for errors; return nil for not-found.
   - Prefer cond-> / threading; use project cond-let when it improves clarity.
   - Co-locate ^:rct/test rich-comment-tests near functions."
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
  (matcher (fn [mr] (when (pred (:val mr)) mr)) :pred))

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
     (when (map? (:val mr))
       (reduce
        (fn [mr' [k mch]]
          (if-let [vmr (some-> (update mr' :val get k) (mch))]
            (-> mr'
                (update :val conj [k (:val vmr)])
                (update :vars merge (:vars vmr)))
            (reduced nil)))
        mr k-matchers)))
   :map))

^:rct/test
(comment
  (def mm (mmap [[:a (mvar 'a (mval 4))] [:b (mpred even?)]]))
  (mm {:val {:a 4 :b 0}})  ;=>
  {:val {:a 4 :b 0} :vars {'a 4}}
  (mm {:val {:a 4 :b 3}}) ;=> nil
  (mm {:val {:a 4 :b 0} :vars {'a 0}}) ;=>
  nil
  )

;;## Sequence matchers

(defn mone
  "Subsequence matcher that consumes exactly one element from `:rest`
   and passes it to `child`. Propagates `:vars` and updates `:rest`."
  [child]
  (matcher
   (fn [mr]
     (when-let [mr' (some->> (:rest mr) first (assoc mr :val) child)]
       (some-> mr (update :vars merge (:vars mr')) (update :rest rest))))
   :subseq))

(defn mrest
  "Subsequence matcher that repeatedly applies `child` to elements in
   `:rest`, capturing all consecutive successes as a vector in
   `:captured` and dropping them from `:rest`."
  ([child]
   (mrest child 0))
  ([child len]
   (matcher
    (fn [mr]
      (let [melems (cond->> (->> (:rest mr) (map #(child (assoc mr :val %))) (take-while identity))
                     (pos? len) (take len))]
        (when (or (zero? len) (>= (count melems) len))
          (-> mr
              (assoc :rest (->> (:rest mr) (drop (count melems))))
              (assoc :captured (->> melems (map :val) (reduce conj [])))))))
    :subseq)))

(defn msubseq
  "Compose a sequence of subsequence matchers. "
  [& matchers]
  (matcher
   (fn [mr]
     (reduce (fn [mr' mch] (or (mch mr') (reduced nil))) mr matchers))
   :subseq))

(def find-first (comp first filter))

(defn mor
  "Branching matcher that tries alternatives in order.
   `kms` is a flat collection `[k1 m1 k2 m2 ...]`; the first successful
   matcher wins and its branch key is stored in `:captured`."
  ([kms]
   (mor kms false))
  ([kms capture?]
   (let [km-pair (partition 2 kms)]
     (matcher
      (fn [mr]
        (when-let [[t mr'] (->> km-pair
                                (map (fn [[k mch]] [k (mch mr)]))
                                (find-first second))]
          (cond-> mr' capture? (assoc :captured t))))
      :or))))

(defn branching
  [pred f coll]
  (loop [fst [] lst (reverse coll)]
    (let [[fst' [h & lst']] (split-with (complement pred) lst)]
      (if h
        (let [v (f h (reverse fst'))]
          (recur (concat fst [v]) lst'))
        (reverse (concat fst lst))))))

^:rct/test
(comment
  (def bf (partial branching #(zero? (mod % 5)) #(apply str % %2)))
  (bf (range 1 8)) ;=> [1 2 3 4 "567"]
  (bf (range 1 3)) ;=> [1 2]
  (bf [1 5 6 8 10 11]) ;=>
  [1 "568" "1011"]
  (bf [5])
  )

(defn moption
  "Optional subsequence matcher"
  ([child]
   (matcher child :subseq ::optional? true)))

(defn mseq
  "Top level sequence matcher.
   Treats the current `:val` as the full sequence, stores it in `:rest`,
   runs `matchers` left-to-right and succeeds only when the whole
   sequence is consumed."
  [matchers]
  (let [matchers (branching #(some-> % meta ::optional?)
                            (fn [c nxt] (mor [:1 (apply msubseq (cons (mone c) nxt)) :0 (apply msubseq nxt)]))
                            matchers)]
    (matcher
     (fn [mr]
       (let [v (:val mr)]
         (when (seqable? v)
           (let [mr (reduce (fn [mr' mch]
                              (or (mch mr') (reduced nil)))
                            (assoc mr :rest v)
                            matchers)]
             (when-not (some-> mr :rest seq)
               (dissoc mr :rest))))))
     :seq)))

^:rct/test
(comment
  ;mone matches a single sequence value
  (def mo (mone (mvar 'a (mval 1))))
  (mo {:val [1] :rest [1]}) ;=>> {:vars {'a 1} :rest []}
  (mo {:val [2] :rest [2]}) ;=> nil
  (mo {:val [] :rest []}) ;=> nil

  ;mseq matches a whole sequence
  (def sm (mseq [mo (mone (mpred zero?)) (mone (mpred odd?))]))
  (sm {:val [1 0 3]}) ;=>> {:vars {'a 1}}
  (sm {:val [1 0 3 2]}) ;=> nil

  ;complex patterns
  (def sm (mseq [(mone (mvar 'b (mval 1))) (mone (mpred zero?)) (mvar 'a (mrest (mpred odd?)))]))
  (sm {:val [1 0 1 3 5]}) ;=>> {:vars {'a [1 3 5]}}
  (sm {:val [1 0 1 3 5 2]}) ;=> nil

  (def sm2 (mseq [(mone (mvar 'b (mval 1))) (msubseq (mone (mpred zero?)))]))
  (sm2 {:val [1 0]})
  (map (mseq [(moption (mvar 'opt (mval 2))) (mone (mval 1))]) [{:val [1]} {:val [2 1]}]) ;=>>
  [{} {:vars {'opt 2}}]
  ((mseq [(mone (mval 4)) (moption (mval 2))]) {:val [4]}) ;=>> {:val [4]}

  ((mseq [(mone (mval 0)) (mvar 'a (mrest (mpred neg?))) (mone (mval 4))]) {:val [0 -1 -3 4]}) ;=>>
  {:vars {'a [-1 -3]}}

  ;mrest support len
  (map (mseq [(mrest (mpred even?) 2)]) [{:val [0]} {:val [0 2]} {:val [0 2 4]}]) ;=>>
  [nil {} nil])


;;## Misc matchers

(defn mf [f child]
  (matcher (fn [mr] (when-let [mr (child mr)] (f mr))) :sub))

(defn msub [f child]
  (mf #(update % :val f) child))

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

^:rct/test
(comment
  ;msub substitute :val and :captured
  (def ms (msub inc (mval 1)))
  (ms {:val 1}) ;=>> {:val 2}

  ;mor find the first matched branch of patterns and capture the type
  (def mo (mvar 'type (mor [:keyword (mpred keyword?)
                            :number (msub str (mpred number?))] true)))
  (mo {:val 3}) ;=>> {:vars {'type :number}}
  )

;;## Pattern
;;
;; Pattern is a mini language using common clojure data, it can translate to matcher functions.

(defn named-var?
  "Parse a symbol as a named pattern variable.
   Returns `[base-name seq?]` when `v` has the `?x`, `??x` or
   forms, otherwise `nil`."
  [v]
  (when (symbol? v)
    (when-let [[_ s n optional?] (re-matches #"(\?[\?]?)([\p{Alnum}-_]+)(\??)" (name v))]
      [(symbol n) (= s "??") (= optional? "?")])))

(defn prefix [sym]
  (when-let [[_ seq? nm] (re-matches #"(\?[\?]?):?([\p{Alnum}-_]*)" (str sym))]
    [(when-not (= nm "") (keyword nm)) (= seq? "??")]))

:rct/test
(comment
  (named-var? '?x3) ;=>> ['x3 false false]
  (named-var? '??x?) ;=>>
  ['x true true]
  )

(defmulti make-matcher
  "Compile a low level pattern description like `[:pred pred]` or
   `[:seq ...]` into a concrete matcher function."
  first)

(defn- scalar-element [x]
  (cond
    (matcher-type x) x
    (fn? x) (mpred x)
    :else (mval x)))

(defn- wrap-element [x]
  (let [mt (matcher-type x)] (cond-> x (not mt) scalar-element (not= mt :subseq) mone)))

(defn- seq-matcher [x]
  (-> (map wrap-element x) mseq))

(defn- map-matcher [x]
  (->> (map (fn [[k v]] [k (scalar-element v)]) x)
       mmap))

(defn mscanner
  ([children]
   (mscanner children nil))
  ([children default]
   (matcher
    (fn [mr]
      (reduce (fn [acc mch] (or (mch acc) default acc)) mr children))
    :scaner)))

(defn vr
  [v]
  (fn [{vars :vars}]
    (walk/postwalk
     (fn [x] (or (and (symbol? x) (get vars x)) x))
     v)))

(def mand #(mscanner % (reduced nil)))

(def rule-basic
  (msub #(make-matcher (rest %))
        (mseq [(mone (mval '?))
               (mone (mvar 'type (mpred keyword?)))
               (mvar 'args (mrest wildcard))])))

(def rule-named
  (msub (fn [sym] (when-let [[var-name _ optional?] (named-var? sym)]
                   (cond->> (mvar var-name wildcard) optional? moption)))
        (mpred named-var?)))

(def rule-wc (msub (fn [_] '(? :*)) (mval '?_)))
(def rule-seq (msub seq-matcher (mpred #(and (vector? %) (not (map-entry? %))))))
(def rule-map (msub map-matcher (mpred map?)))

(defn ptn->matcher
  ([rules ptn]
   (let [srules (mscanner rules)]
     (->> ptn
          (walk/postwalk
           (fn [x] (if-let [mr (srules {:val x})] (:val mr) x)))
          scalar-element))))

(def matcher-of (partial ptn->matcher [rule-map rule-seq rule-wc rule-named rule-basic]))

^:rct/test
(comment
  (map (mscanner [(mseq [(mone (mval 1))]) (mseq [(mone (mval 2))])]) [{:val [1]} {:val [2]} {:val [3]}]) ;=>>
  [{:val [1]} {:val [2]} {:val [3]}]

  ((vr '(* x 5)) {:vars {'x 5}}) ;=>
  '(* 5 5)
  ((matcher-of '(? :var a (? :val 3))) {:val 3}) ;=>>
  {:vars {'a 3}}
  ((matcher-of '[5 ?_ ?a {:a ?c :b {:c ?c}} ?d?]) {:val [5 8 6 {:a 5 :b {:c 5}}]}) ;=>> {:val [5 8] :vars {'a 6}}
  )

(defmethod make-matcher :pred [[_ pred]] (mpred pred))
(defmethod make-matcher :val [[_ v]] (mval v))
(defmethod make-matcher :var [[_ sym child]] (mvar sym child))
(defmethod make-matcher :seq [[_ & children]] (mseq children))
(defmethod make-matcher :map [[_ & children]] (mmap children))
(defmethod make-matcher :* [[&_]] wildcard)

(defmethod make-matcher :1 [[_ child]] (mone child))
(defmethod make-matcher :? [[_ child]] (moption child))

(defmethod make-matcher :sub [[_ f child]] (msub f child))
(defmethod make-matcher :or [[_ & conditions]] (mor conditions))
(defmethod make-matcher :and [[_ & children]] (mand children))
(defmethod make-matcher :rest [[_ child]] (mrest (or child wildcard)))
