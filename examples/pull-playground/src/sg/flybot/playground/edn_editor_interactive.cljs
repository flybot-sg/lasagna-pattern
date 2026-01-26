(ns sg.flybot.playground.edn-editor-interactive
  "Interactive EDN editor with parinfer support.

   This ClojureScript-only module handles the interactive editing behavior:
   - Textarea for input
   - Overlay with syntax highlighting
   - Parinfer integration for auto-balancing"
  (:require [sg.flybot.playground.edn-editor :as edn]
            [parinferish.core :as paren]
            [clojure.string :as str]))

;;=============================================================================
;; Parinfer Processing
;;=============================================================================

(defn apply-parinfer
  "Apply parinfer to text, returning adjusted text.
   Mode can be :indent, :paren, or :smart (default :indent)."
  ([text] (apply-parinfer text :indent nil))
  ([text mode] (apply-parinfer text mode nil))
  ([text mode cursor-info]
   (when text
     (try
       (let [opts (cond-> {:mode mode}
                    cursor-info (merge cursor-info))
             result (paren/parse text opts)]
         (paren/flatten result))
       (catch :default _
         text)))))

;;=============================================================================
;; Editor State Helpers
;;=============================================================================

(defn get-cursor-info
  "Get cursor position from a textarea element."
  [textarea]
  (let [pos (.-selectionStart textarea)
        text (.-value textarea)
        lines-before (str/split (subs text 0 pos) #"\n" -1)]
    {:cursor-line (dec (count lines-before))
     :cursor-column (count (last lines-before))}))

(defn set-cursor-position!
  "Set cursor position in a textarea."
  [textarea pos]
  (set! (.-selectionStart textarea) pos)
  (set! (.-selectionEnd textarea) pos))

(def ^:private paired-chars
  "Map of opening chars to their closing pairs."
  {"(" ")" "[" "]" "{" "}"})

(defn- detect-auto-pair
  "Detect if parinfer added a closing paren and cursor should be positioned inside.
   Returns the position adjustment (0 or -1) based on what was typed."
  [old-text new-text cursor-pos]
  (when (and old-text new-text
             (> (count new-text) (count old-text))
             (> cursor-pos 0))
    (let [;; Character that was just typed (before cursor)
          typed-char (when (< (dec cursor-pos) (count new-text))
                       (subs new-text (dec cursor-pos) cursor-pos))
          ;; Character after cursor in new text
          char-after (when (< cursor-pos (count new-text))
                       (subs new-text cursor-pos (inc cursor-pos)))]
      ;; If we typed an opening paren and the char after is its closing pair,
      ;; cursor should stay between them (no adjustment needed since cursor is already there)
      ;; But if parinfer moved things around, we might need to adjust
      (when (and typed-char (get paired-chars typed-char)
                 (= char-after (get paired-chars typed-char)))
        ;; Cursor is already between the pair, return 0
        0))))

;;=============================================================================
;; Autocomplete Helpers
;;=============================================================================

(defn- get-schema-at-path
  "Navigate schema to get the sub-schema at given path.
   Unwraps vectors (collections) to find the inner map schema."
  [schema path]
  (reduce
   (fn [s key]
     (when (map? s)
       (let [v (get s key)]
         (if (vector? v)
           (first (filter map? v))  ; unwrap collection schema
           v))))
   schema
   path))

(defn- get-keys-at-path
  "Get available keyword keys at a path in the schema."
  [schema path]
  (let [sub-schema (get-schema-at-path schema path)]
    (when (map? sub-schema)
      (->> (keys sub-schema)
           (filter keyword?)
           set))))

(defn- extract-context-path
  "Extract the current nesting path from text before cursor.
   Path is built from keywords that precede opening braces.

   Example: '{:users {:name' -> [:users]
            '{:config {:features {:' -> [:config :features]"
  [text cursor-pos]
  (let [before-cursor (subs text 0 cursor-pos)
        ;; Tokenize: extract keywords and braces
        tokens (re-seq #":[a-zA-Z0-9\-_]+|\{|\}" before-cursor)]
    (loop [tokens tokens
           path []
           pending-kw nil]
      (if-let [tok (first tokens)]
        (cond
          ;; Opening brace - push pending keyword to path
          (= tok "{")
          (recur (rest tokens)
                 (if pending-kw (conj path pending-kw) path)
                 nil)
          ;; Closing brace - pop from path
          (= tok "}")
          (recur (rest tokens)
                 (if (seq path) (pop path) [])
                 nil)
          ;; Keyword - remember it for next opening brace
          :else
          (recur (rest tokens)
                 path
                 (keyword (subs tok 1))))
        path))))

(defn get-current-keyword-prefix
  "Get the partial keyword being typed at cursor position.
   Returns {:prefix string :start-pos int} or nil."
  [text cursor-pos]
  (when (and text (pos? cursor-pos) (<= cursor-pos (count text)))
    (let [before-cursor (subs text 0 cursor-pos)
          ;; Find the start of current keyword (last :)
          last-colon (str/last-index-of before-cursor ":")
          ;; Check if we're in a keyword context (: followed by word chars)
          prefix (when last-colon
                   (let [after-colon (subs before-cursor last-colon)]
                     (when (re-matches #":[a-zA-Z0-9\-_]*" after-colon)
                       after-colon)))]
      (when prefix
        {:prefix prefix
         :start-pos last-colon}))))

(defn filter-completions
  "Filter schema keys by prefix."
  [schema-keys prefix]
  (let [prefix-lower (str/lower-case (or prefix ":"))]
    (->> schema-keys
         (filter #(str/starts-with? (str/lower-case (str %)) prefix-lower))
         (sort-by str)
         (take 10))))

(defn get-caret-coordinates
  "Get approximate pixel coordinates for cursor in textarea."
  [textarea]
  (let [rect (.getBoundingClientRect textarea)
        style (js/getComputedStyle textarea)
        font-size (js/parseFloat (.-fontSize style))
        line-height (or (js/parseFloat (.-lineHeight style)) (* font-size 1.5))
        padding-left (js/parseFloat (.-paddingLeft style))
        padding-top (js/parseFloat (.-paddingTop style))
        text (.-value textarea)
        cursor-pos (.-selectionStart textarea)
        lines-before (str/split (subs text 0 cursor-pos) #"\n" -1)
        current-line (dec (count lines-before))
        current-col (count (last lines-before))
        ;; Approximate character width (monospace)
        char-width (* font-size 0.6)]
    {:x (+ (.-left rect) padding-left (* current-col char-width))
     :y (+ (.-top rect) padding-top (* (inc current-line) line-height))}))

(defn compute-autocomplete
  "Compute autocomplete data for current editor state.
   Context-aware: only shows keys available at current nesting level."
  [textarea schema]
  (let [text (.-value textarea)
        cursor-pos (.-selectionStart textarea)]
    (when-let [{:keys [prefix start-pos]} (get-current-keyword-prefix text cursor-pos)]
      (let [;; Get current path in the pattern structure
            path (extract-context-path text start-pos)
            ;; Get keys available at this path in the schema
            schema-keys (get-keys-at-path schema path)
            completions (when schema-keys
                          (vec (filter-completions schema-keys prefix)))]
        (when (seq completions)
          (let [{:keys [x y]} (get-caret-coordinates textarea)]
            {:completions completions
             :selected 0
             :prefix prefix
             :start-pos start-pos
             :x x
             :y y}))))))

(defn apply-completion
  "Apply selected completion, returns new text and cursor position."
  [text cursor-pos autocomplete]
  (let [{:keys [completions selected start-pos]} autocomplete
        completion (nth completions selected)
        new-text (str (subs text 0 start-pos)
                      (str completion)
                      (subs text cursor-pos))
        new-cursor-pos (+ start-pos (count (str completion)))]
    {:text new-text
     :cursor-pos new-cursor-pos}))

(defn autocomplete-dropdown
  "Render autocomplete dropdown.
   Props: :autocomplete, :on-select, :on-hover"
  [{:keys [autocomplete on-select on-hover]}]
  (when-let [{:keys [completions selected x y]} autocomplete]
    [:div.autocomplete-dropdown
     {:style {:left (str x "px")
              :top (str y "px")}}
     (for [[idx kw] (map-indexed vector completions)]
       [:div.autocomplete-item
        {:replicant/key (str kw)
         :class (when (= idx selected) "selected")
         :on {:mousedown (fn [e]
                           (.preventDefault e)
                           (on-select idx))
              :mouseenter #(on-hover idx)}}
        (str kw)])]))

;;=============================================================================
;; Editor Component
;;=============================================================================

(defn edn-editor
  "Interactive EDN editor with syntax highlighting, parinfer, and autocomplete.

   Props:
   - :value - Current text value
   - :on-change - Callback (fn [new-value]) when text changes
   - :placeholder - Placeholder text when empty
   - :class - Additional CSS class
   - :parinfer-mode - :indent, :paren, :smart, or nil to disable (default :indent)
   - :read-only? - If true, editor is read-only
   - :schema - Schema for autocomplete suggestions
   - :autocomplete - Current autocomplete state (from app state)
   - :on-autocomplete - Callback (fn [autocomplete-data]) to show autocomplete
   - :on-autocomplete-hide - Callback to hide autocomplete
   - :on-autocomplete-select - Callback (fn [idx]) when item selected
   - :on-autocomplete-move - Callback (fn [direction]) for arrow keys (+1/-1)"
  [{:keys [value on-change placeholder class parinfer-mode read-only? schema
           autocomplete on-autocomplete on-autocomplete-hide
           on-autocomplete-select on-autocomplete-move]
    :or {parinfer-mode :indent}}]
  (let [editor-id (str "edn-editor-" (random-uuid))]
    [:div.edn-editor {:class class}
     ;; Hidden textarea for actual editing
     [:textarea.edn-editor-input
      {:id editor-id
       :value (or value "")
       :placeholder placeholder
       :spellCheck false
       :readOnly read-only?
       :on {:input (fn [e]
                     (when on-change
                       (let [textarea (.-target e)
                             new-text (.-value textarea)
                             cursor-pos (.-selectionStart textarea)
                             cursor (get-cursor-info textarea)
                             ;; Apply parinfer if enabled
                             processed (if parinfer-mode
                                         (apply-parinfer new-text parinfer-mode cursor)
                                         new-text)
                             ;; Check if parinfer added closing parens
                             text-grew? (and processed (> (count processed) (count new-text)))
                             ;; Detect what was typed
                             typed-char (when (and (> cursor-pos 0) (<= cursor-pos (count new-text)))
                                          (subs new-text (dec cursor-pos) cursor-pos))
                             ;; Check if we need to position cursor inside paired parens
                             _should-adjust? (and text-grew?
                                                  typed-char
                                                  (get paired-chars typed-char))]
                         ;; Update textarea if parinfer changed something
                         (when (and parinfer-mode (not= new-text processed))
                           (set! (.-value textarea) processed)
                           ;; Keep cursor at same position (inside the pair)
                           (set-cursor-position! textarea cursor-pos))
                         (on-change (or processed new-text))
                         ;; Show autocomplete if schema available and typing a keyword
                         (when (and schema on-autocomplete)
                           (if-let [ac-data (compute-autocomplete textarea schema)]
                             (on-autocomplete ac-data)
                             (when on-autocomplete-hide (on-autocomplete-hide)))))))
            :keydown (fn [e]
                       (when autocomplete
                         (let [key (.-key e)]
                           (case key
                             "ArrowDown" (do (.preventDefault e)
                                             (when on-autocomplete-move
                                               (on-autocomplete-move 1)))
                             "ArrowUp" (do (.preventDefault e)
                                           (when on-autocomplete-move
                                             (on-autocomplete-move -1)))
                             ("Tab" "Enter")
                             (do (.preventDefault e)
                                 (let [textarea (.-target e)
                                       {:keys [text cursor-pos]} (apply-completion
                                                                  (.-value textarea)
                                                                  (.-selectionStart textarea)
                                                                  autocomplete)]
                                   (set! (.-value textarea) text)
                                   (set-cursor-position! textarea cursor-pos)
                                   (on-change text)
                                   (when on-autocomplete-hide (on-autocomplete-hide))))
                             "Escape" (when on-autocomplete-hide (on-autocomplete-hide))
                             nil))))
            :blur (fn [_]
                    ;; Delay to allow click on autocomplete item
                    (when on-autocomplete-hide
                      (js/setTimeout on-autocomplete-hide 150)))
            :scroll (fn [e]
                       ;; Sync scroll position with overlay
                      (let [textarea (.-target e)
                            overlay (.querySelector (.-parentElement textarea) ".edn-editor-overlay")]
                        (when overlay
                          (set! (.-scrollTop overlay) (.-scrollTop textarea))
                          (set! (.-scrollLeft overlay) (.-scrollLeft textarea)))))}}]
     ;; Overlay with syntax highlighting (positioned on top, pointer-events: none)
     [:div.edn-editor-overlay
      [:pre.edn-editor-code
       (if (str/blank? value)
         [:span.edn-placeholder placeholder]
         (edn/highlight-edn value))]]
     ;; Autocomplete dropdown
     (when autocomplete
       (autocomplete-dropdown
        {:autocomplete autocomplete
         :on-select (fn [idx]
                      (let [textarea (js/document.querySelector (str "#" editor-id))
                            updated-ac (assoc autocomplete :selected idx)
                            {:keys [text cursor-pos]} (apply-completion
                                                       (.-value textarea)
                                                       (.-selectionStart textarea)
                                                       updated-ac)]
                        (set! (.-value textarea) text)
                        (set-cursor-position! textarea cursor-pos)
                        (.focus textarea)
                        (on-change text)
                        (when on-autocomplete-hide (on-autocomplete-hide))))
         :on-hover (fn [idx]
                     (when on-autocomplete-select
                       (on-autocomplete-select idx)))}))]))

;;=============================================================================
;; Schema Viewer with Hover Documentation
;;=============================================================================

(defonce tooltip-state (atom nil))

(defn- show-tooltip! [token element schema]
  (when-let [kw (edn/keyword-from-token token)]
    (when-let [doc-info (edn/lookup-field-doc schema kw)]
      (let [rect (.getBoundingClientRect element)]
        (reset! tooltip-state
                {:token token
                 :doc doc-info
                 :x (.-left rect)
                 :y (+ (.-bottom rect) 4)})))))

(defn- hide-tooltip! []
  (reset! tooltip-state nil))

(defn- tooltip-view []
  (when-let [{:keys [token doc x y]} @tooltip-state]
    (let [{:keys [doc example deprecated type since]} doc]
      [:div.schema-tooltip
       {:style {:left (str x "px")
                :top (str y "px")}}
       [:div.tooltip-header token]
       (when type
         [:div.tooltip-type (pr-str type)])
       (when doc
         [:div.tooltip-doc doc])
       (when example
         [:div.tooltip-example
          [:span.tooltip-label "Example: "]
          [:code (pr-str example)]])
       (when deprecated
         [:div.tooltip-deprecated
          (if (string? deprecated)
            (str "⚠️ Deprecated: " deprecated)
            "⚠️ Deprecated")])
       (when since
         [:div.tooltip-since
          [:span.tooltip-label "Since: "] since])])))

(defn schema-viewer
  "Read-only schema viewer with syntax highlighting and hover documentation.
   Shows tooltips when hovering over schema fields.

   Props:
   - :value - EDN string of schema
   - :schema - Original schema value (with metadata for docs)
   - :placeholder - Placeholder text
   - :class - CSS class
   - :loading? - Show loading state
   - :error - Error message"
  [{:keys [value schema placeholder class loading? error]}]
  (cond
    loading?
    [:div.schema-viewer {:class class}
     [:div.schema-loading "Loading..."]]

    error
    [:div.schema-viewer {:class class}
     [:div.schema-error error]]

    (str/blank? value)
    [:div.schema-viewer {:class class}
     [:div.schema-empty (or placeholder "No schema loaded")]]

    :else
    [:div.schema-viewer {:class class
                         :on {:mouseleave (fn [_] (hide-tooltip!))}}
     [:pre.schema-code
      (edn/highlight-edn value
                         {:on-hover (fn [token]
                                      (when-let [el (js/document.querySelector
                                                     (str "[data-token=\"" token "\"]"))]
                                        (show-tooltip! token el schema)))
                          :on-leave hide-tooltip!})]
     (tooltip-view)]))
