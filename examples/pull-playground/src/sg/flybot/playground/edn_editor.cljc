(ns sg.flybot.playground.edn-editor
  "EDN editor component with syntax highlighting and rainbow parens.

   Uses parinferish to parse EDN/Clojure code and render it with
   syntax highlighting. Supports rainbow parentheses for visual
   nesting depth indication."
  (:require [parinferish.core :as paren]
            [clojure.string :as str]))

;;=============================================================================
;; Token Classification
;;=============================================================================

(def ^:private rainbow-colors
  "Colors for rainbow parens - cycles through these based on nesting depth."
  ["rainbow-1" "rainbow-2" "rainbow-3" "rainbow-4" "rainbow-5" "rainbow-6"])

(defn- paren-char? [s]
  (contains? #{"(" ")" "[" "]" "{" "}"} s))

(defn- open-paren? [s]
  (contains? #{"(" "[" "{"} s))

(defn- close-paren? [s]
  (contains? #{")" "]" "}"} s))

(defn- classify-token
  "Classify a token string for syntax highlighting."
  [token-str]
  (cond
    (nil? token-str) :whitespace
    (str/blank? token-str) :whitespace
    (paren-char? token-str) :paren
    (str/starts-with? token-str ";") :comment
    (str/starts-with? token-str "\"") :string
    (str/starts-with? token-str ":") :keyword
    (str/starts-with? token-str "?") :variable  ; Pull pattern variables
    (re-matches #"-?\d+\.?\d*" token-str) :number
    (contains? #{"true" "false" "nil"} token-str) :literal
    :else :symbol))

;;=============================================================================
;; Parsing
;;=============================================================================

(defn- flatten-parsed
  "Flatten parinferish parsed tree into a sequence of [type content] tuples."
  [parsed]
  (letfn [(walk [node]
            (cond
              (string? node) [[node]]
              (vector? node)
              (let [[tag & children] node]
                (cond
                  ;; Skip metadata tags, just process children
                  (keyword? tag) (mapcat walk children)
                  ;; String content
                  (string? tag) [[tag]]
                  :else (mapcat walk node)))
              :else []))]
    (mapcat walk parsed)))

(defn parse-edn
  "Parse EDN string into tokens for highlighting.
   Returns a seq of {:token string :type keyword :depth int}."
  [text]
  (when (and text (not (str/blank? text)))
    (try
      (let [parsed (paren/parse text)
            tokens (flatten-parsed parsed)]
        (loop [remaining tokens
               depth 0
               result []]
          (if (empty? remaining)
            result
            (let [[token-str] (first remaining)
                  token-type (classify-token token-str)
                  new-depth (cond
                              (and (= token-type :paren) (open-paren? token-str))
                              (inc depth)

                              (and (= token-type :paren) (close-paren? token-str))
                              depth  ; Use current depth for closing paren

                              :else depth)
                  ;; For closing parens, use depth-1 for color matching
                  effective-depth (if (and (= token-type :paren) (close-paren? token-str))
                                    (dec depth)
                                    (dec new-depth))
                  next-depth (if (and (= token-type :paren) (close-paren? token-str))
                               (dec depth)
                               new-depth)]
              (recur (rest remaining)
                     next-depth
                     (conj result {:token token-str
                                   :type token-type
                                   :depth (max 0 effective-depth)}))))))
      (catch #?(:clj Exception :cljs :default) _
        ;; On parse error, return plain text
        [{:token text :type :plain :depth 0}]))))

;;=============================================================================
;; Rendering
;;=============================================================================

(defn- token-class
  "Get CSS class for a token based on its type and depth."
  [{:keys [type depth]}]
  (case type
    :paren (str "token-paren " (get rainbow-colors (mod depth (count rainbow-colors))))
    :comment "token-comment"
    :string "token-string"
    :keyword "token-keyword"
    :variable "token-variable"
    :number "token-number"
    :literal "token-literal"
    :symbol "token-symbol"
    :whitespace "token-whitespace"
    "token-plain"))

(defn highlight-edn
  "Render EDN text with syntax highlighting as hiccup.
   Returns a [:span ...] with highlighted tokens."
  [text]
  (if (or (nil? text) (str/blank? text))
    [:span.edn-empty]
    (let [tokens (parse-edn text)]
      (into [:span.edn-highlighted]
            (for [{:keys [token type] :as tok} tokens]
              (if (= type :whitespace)
                token  ; Preserve whitespace as-is
                [:span {:class (token-class tok)} token]))))))

;;=============================================================================
;; Editor Component (read-only display)
;;=============================================================================

(defn edn-display
  "Read-only EDN display with syntax highlighting.
   For editable version, use edn-editor from edn-editor-interactive.cljs"
  [{:keys [value placeholder class]}]
  [:div.edn-display {:class class}
   (if (str/blank? value)
     [:span.edn-placeholder placeholder]
     [:pre.edn-code (highlight-edn value)])])

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; classify-token identifies keywords
  (classify-token ":foo") ;=> :keyword

  ;; classify-token identifies variables
  (classify-token "?name") ;=> :variable

  ;; classify-token identifies numbers
  (classify-token "42") ;=> :number
  (classify-token "-3.14") ;=> :number

  ;; classify-token identifies parens
  (classify-token "(") ;=> :paren
  (classify-token "}") ;=> :paren

  ;; classify-token identifies strings
  (classify-token "\"hello\"") ;=> :string

  ;; classify-token identifies comments
  (classify-token "; comment") ;=> :comment

  ;; paren-char? works
  (paren-char? "(") ;=> true
  (paren-char? "foo") ;=> false

  ;; open-paren? works
  (open-paren? "(") ;=> true
  (open-paren? ")") ;=> false

  ;; highlight-edn returns hiccup
  (first (highlight-edn "{:a 1}"))) ;=> :span.edn-highlighted)
