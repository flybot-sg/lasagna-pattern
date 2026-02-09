(ns sg.flybot.playground.ui.core.views.edn-editor-interactive.editor
  "EDN editor component with syntax highlighting and rainbow parens.

   Uses parinferish to parse EDN/Clojure code and render it with
   syntax highlighting. Supports rainbow parentheses for visual
   nesting depth indication."
  (:require [parinferish.core :as paren]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

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
  "Get CSS class for a token based on its type and depth.
   Returns a vector of class names for Replicant compatibility."
  [{:keys [type depth]}]
  (case type
    :paren ["token-paren" (get rainbow-colors (mod depth (count rainbow-colors)))]
    :comment ["token-comment"]
    :string ["token-string"]
    :keyword ["token-keyword"]
    :variable ["token-variable"]
    :number ["token-number"]
    :literal ["token-literal"]
    :symbol ["token-symbol"]
    :whitespace ["token-whitespace"]
    ["token-plain"]))

(defn highlight-edn
  "Render EDN text with syntax highlighting as hiccup.
   Returns a [:span ...] with highlighted tokens.

   Options:
   - :on-hover - callback (fn [token]) when hovering a keyword
   - :on-leave - callback (fn []) when leaving a token"
  ([text] (highlight-edn text nil))
  ([text {:keys [on-hover on-leave]}]
   (if (or (nil? text) (str/blank? text))
     [:span.edn-empty]
     (let [tokens (parse-edn text)]
       (into [:span.edn-highlighted]
             (for [{:keys [token type] :as tok} tokens]
               (if (= type :whitespace)
                 token  ; Preserve whitespace as-is
                 (let [base-attrs {:class (token-class tok)}
                       ;; Add hover handlers for keywords (schema keys)
                       attrs (if (and (= type :keyword) on-hover)
                               (assoc base-attrs
                                      :data-token token
                                      :on (cond-> {}
                                            on-hover (assoc :mouseenter #(on-hover token))
                                            on-leave (assoc :mouseleave (fn [_] (on-leave)))))
                               base-attrs)]
                   [:span attrs token]))))))))

;;=============================================================================
;; Editor Component (read-only display)
;;=============================================================================

(defn pretty-str
  "Pretty-print a Clojure value to a string."
  [v]
  (if (nil? v)
    "nil"
    (with-out-str (pp/pprint v))))

;;=============================================================================
;; Schema Documentation Lookup
;;=============================================================================

(defn keyword-from-token
  "Parse a keyword token string like ':name' into a keyword."
  [token-str]
  (when (and token-str (str/starts-with? token-str ":"))
    (keyword (subs token-str 1))))

(defn- malli-entry-props
  "Extract properties from a Malli map entry.
   Entry format: [:key schema] or [:key props schema]"
  [entry]
  (when (and (vector? entry) (>= (count entry) 2))
    (let [[_key second-elem] entry]
      (when (map? second-elem)
        second-elem))))

(defn- malli-entry-schema
  "Extract the schema from a Malli map entry.
   Entry format: [:key schema] or [:key props schema]"
  [entry]
  (when (and (vector? entry) (>= (count entry) 2))
    (if (= (count entry) 2)
      (second entry)
      (last entry))))

(defn- malli-map-entries
  "Get entries from a Malli hiccup map schema form.
   Returns entries after the type keyword and optional props map."
  [schema-form]
  (when (and (vector? schema-form) (= :map (first schema-form)))
    (let [rest-elems (rest schema-form)]
      ;; Skip properties map if present at the start
      (if (map? (first rest-elems))
        (rest rest-elems)
        rest-elems))))

(defn- unwrap-schema-form
  "Unwrap collection types to get the inner schema form."
  [schema-form]
  (when (vector? schema-form)
    (case (first schema-form)
      (:vector :sequential :set :seqable) (second schema-form)
      schema-form)))

(defn- search-schema-for-field
  "Recursively search a Malli hiccup schema for a field's properties."
  [schema-form field-key]
  (when (vector? schema-form)
    (let [type-kw (first schema-form)]
      (case type-kw
        :map
        (let [entries (malli-map-entries schema-form)]
          ;; First check direct match in this map
          (or (some (fn [entry]
                      (when (and (vector? entry) (= (first entry) field-key))
                        (or (malli-entry-props entry) {})))
                    entries)
              ;; Otherwise search nested schemas
              (some (fn [entry]
                      (when-let [child-schema (malli-entry-schema entry)]
                        (search-schema-for-field child-schema field-key)))
                    entries)))
        ;; Collection types - unwrap and search inner schema
        (:vector :sequential :set :seqable)
        (when-let [inner (second schema-form)]
          (search-schema-for-field inner field-key))
        ;; Other types - no fields to search
        nil))))

(defn lookup-field-doc
  "Look up documentation for a field in a Malli hiccup schema.

   Malli hiccup format uses inline properties on each field entry:
   [:map {:doc \"...\"}
    [:field1 {:doc \"...\" :example ...} :type]
    [:nested {:doc \"...\"}
     [:map
      [:inner {:doc \"...\"} :type]]]]

   Searches recursively through nested map schemas.
   Returns map with :doc, :example, :deprecated, :since or nil."
  [schema field-key]
  (when (and schema field-key)
    (let [;; Get the schema form - vector form expected (sent over wire)
          ;; In ClojureScript, schema is always the hiccup form
          ;; In Clojure, try to extract form from Malli schema object
          schema-form (if (vector? schema)
                        schema
                        #?(:clj (try
                                  ((requiring-resolve 'malli.core/form) schema)
                                  (catch Exception _ nil))
                           :cljs nil))
          field-props (when schema-form
                        (search-schema-for-field schema-form field-key))]
      (when (and field-props (or (:doc field-props)
                                 (:example field-props)
                                 (:deprecated field-props)
                                 (:since field-props)))
        field-props))))

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
  (first (highlight-edn "{:a 1}")) ;=> :span.edn-highlighted

  ;; keyword-from-token parses keywords
  (keyword-from-token ":foo") ;=> :foo
  (keyword-from-token "foo") ;=> nil

  ;; lookup-field-doc finds top-level fields in Malli hiccup format
  (let [schema [:map [:name {:doc "User name" :example "Alice"} :string]]]
    (:doc (lookup-field-doc schema :name)))
  ;=> "User name"

  ;; lookup-field-doc finds nested fields recursively
  (let [schema [:map
                [:user {:doc "User info"}
                 [:map
                  [:email {:doc "Email address"} :string]]]]]
    (:doc (lookup-field-doc schema :email)))
  ;=> "Email address"

  ;; lookup-field-doc finds fields inside vectors
  (let [schema [:map
                [:users {:doc "User list"}
                 [:vector
                  [:map
                   [:id {:doc "User ID" :example 1} :int]]]]]]
    (:doc (lookup-field-doc schema :id)))
  ;=> "User ID"

  ;; lookup-field-doc returns nil for unknown fields
  (let [schema [:map [:name {:doc "Name"} :string]]]
    (lookup-field-doc schema :unknown))
  ;=> nil

  ;; lookup-field-doc returns nil when no doc properties
  (let [schema [:map [:name :string]]]
    (lookup-field-doc schema :name)))
  ;=> nil)
