(ns sg.flybot.playground.local
  "Local pattern execution using the pattern library with SCI."
  (:require [cljs.reader :as reader]
            [sci.core :as sci]
            [sg.flybot.pullable.impl :as impl]))

;; SCI context for sandboxed evaluation
(def ^:private sci-ctx
  (sci/init {:namespaces {'clojure.core {'pos? pos?
                                         'neg? neg?
                                         'zero? zero?
                                         'even? even?
                                         'odd? odd?
                                         'string? string?
                                         'number? number?
                                         'keyword? keyword?
                                         'symbol? symbol?
                                         'map? map?
                                         'vector? vector?
                                         'seq? seq?
                                         'set? set?
                                         'nil? nil?
                                         'some? some?
                                         'empty? empty?
                                         'count count
                                         '= =
                                         'not= not=
                                         '< <
                                         '> >
                                         '<= <=
                                         '>= >=
                                         'identity identity}}}))

(defn- sci-resolve [sym]
  (sci/eval-form sci-ctx sym))

(defn- sci-eval [form]
  (sci/eval-form sci-ctx form))

(defn execute
  "Execute a pattern against data locally.

   pattern-str - EDN string of the pattern
   data-str    - EDN string of the data

   Returns bindings map (symbol -> value) on success,
   or {:error \"message\"} on failure."
  [pattern-str data-str]
  (try
    (let [pattern (reader/read-string pattern-str)
          data (reader/read-string data-str)
          matcher (impl/compile-pattern pattern {:resolve sci-resolve
                                                 :eval sci-eval})
          result (matcher (impl/vmr data))]
      (if (impl/failure? result)
        {:error (str "Match failed: " (:reason result)
                     (when (seq (:path result))
                       (str " at path " (:path result))))}
        ;; Return vars bindings directly (spec-compliant format)
        (:vars result)))
    (catch :default e
      {:error (str "Error: " (.-message e))})))

^:rct/test
(comment
  ;; Basic binding works - returns vars directly
  (execute "{:name ?n}" "{:name \"Alice\"}")
  ;=>> {'n "Alice"}

  ;; Multiple bindings - returns vars directly
  (execute "{:a ?a :b ?b}" "{:a 1 :b 2 :c 3}")
  ;=>> {'a 1 'b 2}

  ;; Parse error returns error
  (:error (execute "{:name ?n" "{:name \"Alice\"}"))
  ;=>> string?

  ;; Match failure returns error
  (:error (execute "{:name ?n :age ?a}" "{:name \"Alice\"}")))
  ;=>> string?)
