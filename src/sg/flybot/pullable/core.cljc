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
  identity)

(defn mvar
  "a matcher binds `child` matchers result to a symbol `sym` when `child` success,
   it also checks if the new result equals to known value of the variable, if not,
   the match fails.

  `child` matcher can specify a `:captured` value in mr, this matcher will use it
   instead of `:val`."
  [sym child]
  (with-meta
    (fn [mr]
      (when-let [mr (child mr)]
        (let [new-v (or (:captured mr) (:val mr))
              old-v (get-in mr [:vars sym] ::not-found)
              mr (dissoc mr :captured)]
          (condp = old-v
            ::not-found (assoc-in mr [:vars sym] new-v) ;a fresh match
            new-v mr                   ;current value agrees with existing value
            nil))))
    (meta child)))

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
  [child]
  (matcher
   (fn [mr]
     (when-let [v (some->> (:rest mr) first)]
       (when-let [mr' (->> (assoc mr :val v) child)]
         (some-> (assoc mr :captured [v]) (update :rest rest) (update :vars merge (:vars mr'))))))
   :subseq ::length 1))

(defn mrest
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
     (reduce (fn [mr' mch] (if-let [mr'' (mch mr')]
                            (update mr'' :captured #(concat (:captured mr') %))
                            (reduced nil)))
             mr matchers))
   :subseq))

(def find-first (comp first filter))

(defn mor
  ([children]
   (matcher
    (fn [mr]
      (some->> (map #(% mr) children) (find-first identity)))
    :or)))

^:rct/test
(comment
  (def mth (mvar 'a (mor [(mval 3) (mval 4)])))
  (mth {:val 3}) ;=>> {:vars {'a 3}}
  (mth {:val 5}) ;=>> nil
  )

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
   (matcher child :subseq ::optional (fn [c nxt] (mor [(apply msubseq (cons (mone c) nxt)) (apply msubseq nxt)])))))

(defn- optional [x] (some-> x meta ::optional))

(def mnone
  (matcher
   (fn [mr] (when-not (some-> mr :rest seq) (dissoc mr :rest)))
   :subseq))

(defn mlazy-subseq
  ([child min-len max-len greedy?]
   (mlazy-subseq child min-len max-len greedy? nil))
  ([child min-len max-len greedy? var-sym]
   (matcher
    child
    :subseq ::optional (fn [c nxt]
                         (let [mth (->> (if greedy? (range max-len (dec min-len) -1) (range min-len (inc max-len)))
                                        (map (fn [len] (if (zero? len) (apply msubseq nxt) (apply msubseq (mrest c len) nxt))))
                                        (mor))]
                           (cond->> mth var-sym (mvar var-sym)))))))

(defn mseq
  "Top level sequence matcher.
   Treats the current `:val` as the full sequence, stores it in `:rest`,
   runs `matchers` left-to-right and succeeds only when the whole
   sequence is consumed."
  [matchers]
  (let [matchers (branching optional (fn [c nxt] ((optional c) c nxt)) matchers)]
    (matcher
     (fn [mr]
       (let [v (:val mr)]
         (when (seqable? v)
           (-> (reduce (fn [mr' mch]
                         (or (mch mr') (reduced nil)))
                       (assoc mr :rest v)
                       matchers)
               (dissoc :rest)))))
     :seq)))

^:rct/test
(comment
  ;;mone matches a single sequence value
  (def mo (mvar 'a (mone (mval 1))))
  (mo {:val [1] :rest [1]})             ;=>> {:vars {'a [1]} :rest []}
  (mo {:val [2] :rest [2]})             ;=> nil
  (mo {:val [] :rest []})               ;=> nil

  ;;mseq matches a whole sequence
  (def sm (mseq [(mone (mvar 'a (mval 1))) (mone (mpred zero?)) (mone (mpred odd?)) mnone]))
  (sm {:val [1 0 3]})                   ;=>> {:vars {'a 1}}
  (sm {:val [1 0 3 2]})                 ;=> nil

  ;;complex patterns
  (def sm (mseq [(mone (mvar 'b (mval 1))) (mone (mpred zero?)) (mvar 'a (mrest (mpred odd?))) mnone]))
  (sm {:val [1 0 1 3 5]})               ;=>> {:vars {'a [1 3 5]}}
  (sm {:val [1 0 1 3 5 2]})             ;=> nil

  ;;moption support
  (map (mseq [(moption (mvar 'opt (mval 2))) (mone (mval 1))]) [{:val [1]} {:val [2 1]}]) ;=>>
  [{} {:vars {'opt 2}}]
  ((mseq [(mone (mval 4)) (moption (mval 2))]) {:val [4]}) ;=>> {:val [4]}

  ;;mrest support len
  (map (mseq [(mrest (mpred even?) 2) mnone]) [{:val [0]} {:val [0 2]} {:val [0 2 4]}]) ;=>>
  [nil {} nil]

  ;;TODO wrong result
  ((mseq [(mvar 'a (msubseq (mone wildcard) (mone wildcard))) (mone (mval 10)) mnone]) {:val [5 6 10]}) ;=>>
  {:vars {'a [5 6]}}
  ;;more branching
  (def mth (mseq [(mlazy-subseq (mval 10) 0 2 false 'a)  mnone]))
  (map mth [{:val []} {:val [10 10]} {:val [10 10 10]}]) ;=>>
  [{:val []} {:val [10 10]} nil]
  ;;greedy
  (map (mseq [(mlazy-subseq (mval 10) 0 2 true 'a) (mone (mval 20)) mnone])
       [{:val [10 20]} {:val [10 10 20]} {:val [10 10 10 20]}]) ;=>>
  [{:vars {'a [10]}} {:vars {'a [10 10]}} nil])

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
      [(when-not (= "_" n) (symbol n)) (cond-> #{} (= optional? "?") (conj :optional) (= s "??") (conj :seq))])))

:rct/test
(comment
  (named-var? '?x3) ;=>>
  ['x3 #{}]
  (named-var? '?x?) ;=>>
  ['x #{:optional}]
  (named-var? '?_)
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
  (->> (map wrap-element x) (concat [mnone]) mseq))

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

(comment
  :dbg
  ((mseq [(mlazy-subseq wildcard 0 3 false)]) {:val [1 1 1]}))

(def rule-named
  (msub (fn [sym] (when-let [[var-name flags] (named-var? sym)]
                    (cond->> wildcard
                      (flags :seq) (#(mrest % 0))
                      var-name (mvar var-name)
                      (flags :optional) moption)))
        (mpred named-var?)))

(def rule-seq (msub seq-matcher (mpred #(and (vector? %) (not (map-entry? %))))))
(def rule-map (msub map-matcher (mpred map?)))

(defn ptn->matcher
  ([rules ptn]
   (let [srules (mscanner rules)]
     (->> ptn
          (walk/postwalk
           (fn [x] (if-let [mr (srules {:val x})] (:val mr) x)))
          scalar-element))))

(def matcher-of (partial ptn->matcher [rule-map rule-seq rule-named rule-basic]))

^:rct/test
(comment
  (map (mscanner [(mseq [(mone (mval 1)) mnone]) (mseq [(mone (mval 2)) mnone])]) [{:val [1]} {:val [2]} {:val [3]}]) ;=>>
  [{:val [1]} {:val [2]} {:val [3]}]

  ((vr '(* x 5)) {:vars {'x 5}}) ;=>
  '(* 5 5)
  ((matcher-of '(? :var a (? :val 3))) {:val 3}) ;=>>
  {:vars {'a 3}}
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
