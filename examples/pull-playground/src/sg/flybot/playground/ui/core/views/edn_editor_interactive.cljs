(ns sg.flybot.playground.ui.core.views.edn-editor-interactive
  "Interactive EDN editor with parinfer support.

   This ClojureScript-only module handles the interactive editing behavior:
   - Textarea for input
   - Overlay with syntax highlighting
   - Parinfer integration for auto-balancing"
  (:require [sg.flybot.playground.ui.core.views.edn-editor-interactive.editor :as edn]
            [parinferish.core :as paren]
            [clojure.string :as str]
            [malli.core :as m]))

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

;;=============================================================================
;; Autocomplete Helpers - Malli Schema Support
;;=============================================================================

(def ^:private schema-cache (atom {}))

(defn- ->malli-schema
  "Convert schema form to Malli schema. Memoized to avoid repeated parsing."
  [schema-form]
  (if-let [cached (get @schema-cache schema-form)]
    cached
    (try
      (let [schema (m/schema schema-form)]
        (swap! schema-cache assoc schema-form schema)
        schema)
      (catch :default _ nil))))

(defn- malli-map-keys
  "Get all keyword keys from a Malli map schema."
  [schema]
  (when (and schema (= :map (m/type schema)))
    (->> (m/entries schema)
         (map first)
         (filter keyword?)
         set)))

(defn- malli-get-child
  "Get the child schema for a key in a Malli map schema.
   Malli entries can be [key schema] or [key props schema]."
  [schema key]
  (when (and schema (= :map (m/type schema)))
    (some (fn [entry]
            (when (= (first entry) key)
              (last entry)))  ; schema is always last element
          (m/entries schema))))

(defn- unwrap-schema
  "Unwrap Malli schema wrappers and collection types to get inner map schema.
   Handles :malli.core/val wrappers and :vector/:sequential collections."
  [schema]
  (when schema
    (let [t (m/type schema)]
      (case t
        ;; Malli wraps entry values in :malli.core/val
        :malli.core/val
        (recur (first (m/children schema)))
        ;; Collection types - unwrap to inner schema
        (:vector :sequential :set :seqable)
        (recur (first (m/children schema)))
        ;; Return as-is for maps and other types
        schema))))

(defn- get-schema-at-path
  "Navigate Malli schema to get the sub-schema at given path."
  [schema-form path]
  (when-let [schema (->malli-schema schema-form)]
    (reduce
     (fn [s key]
       (when s
         (some-> (malli-get-child s key)
                 unwrap-schema)))
     schema
     path)))

(defn- get-keys-at-path
  "Get available keyword keys at a path in the schema."
  [schema-form path]
  (if (empty? path)
    ;; Root level - get keys from the root schema
    (when-let [schema (->malli-schema schema-form)]
      (malli-map-keys schema))
    ;; Nested path - navigate to the sub-schema
    (when-let [sub-schema (get-schema-at-path schema-form path)]
      (malli-map-keys sub-schema))))

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
    (when-let [{:keys [prefix start-pos]} (edn/get-current-keyword-prefix text cursor-pos)]
      (let [path (edn/extract-context-path text start-pos)
            schema-keys (get-keys-at-path schema path)
            completions (when schema-keys
                          (vec (edn/filter-completions schema-keys prefix)))]
        (when (seq completions)
          (let [{:keys [x y]} (get-caret-coordinates textarea)]
            {:completions completions
             :selected 0
             :prefix prefix
             :start-pos start-pos
             :x x
             :y y}))))))

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
;; Schema Hover Tooltips
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

;;=============================================================================
;; Editor Component
;;=============================================================================

(defn edn-editor
  "Interactive EDN editor with syntax highlighting, parinfer, autocomplete, and hover tooltips.

   Props:
   - :value - Current text value
   - :on-change - Callback (fn [new-value]) when text changes
   - :placeholder - Placeholder text when empty
   - :class - Additional CSS class
   - :parinfer-mode - :indent, :paren, :smart, or nil to disable (default :indent)
   - :read-only? - If true, editor is read-only
   - :schema - Schema for autocomplete suggestions and hover tooltips
   - :autocomplete - Current autocomplete state (from app state)
   - :on-autocomplete - Callback (fn [autocomplete-data]) to show autocomplete
   - :on-autocomplete-hide - Callback to hide autocomplete
   - :on-autocomplete-select - Callback (fn [idx]) when item selected
   - :on-autocomplete-move - Callback (fn [direction]) for arrow keys (+1/-1)
   - :editor-id - Optional stable editor ID (for autocomplete coordination)
   - :hover-tooltips? - Enable schema documentation tooltips on hover (default false)"
  [{:keys [value on-change placeholder class parinfer-mode read-only? schema
           autocomplete on-autocomplete on-autocomplete-hide
           on-autocomplete-select on-autocomplete-move editor-id hover-tooltips?]
    :or {parinfer-mode :indent}}]
  (let [editor-id (or editor-id (str "edn-editor-" (random-uuid)))]
    [:div.edn-editor {:class class
                      :data-editor-id editor-id}
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
                                         new-text)]
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
                       (let [key (.-key e)]
                         ;; Auto-close string quotes
                         (when (= key "\"")
                           (let [textarea (.-target e)
                                 pos (.-selectionStart textarea)
                                 text (.-value textarea)
                                 next-char (when (< pos (count text))
                                             (subs text pos (inc pos)))
                                 quote-count (count (re-seq #"\"" (subs text 0 pos)))
                                 inside-string? (odd? quote-count)]
                             (cond
                               (= next-char "\"")
                               (do (.preventDefault e)
                                   (set-cursor-position! textarea (inc pos)))
                               (not inside-string?)
                               (do (.preventDefault e)
                                   (let [new-text (str (subs text 0 pos)
                                                       "\"\"" (subs text pos))]
                                     (set! (.-value textarea) new-text)
                                     (set-cursor-position! textarea (inc pos))
                                     (when on-change (on-change new-text)))))))
                         ;; Autocomplete keyboard navigation
                         (when autocomplete
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
                                       {:keys [text cursor-pos]} (edn/apply-completion
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
     ;; When hover-tooltips? is enabled, keywords get pointer-events: auto for hover
     [:div.edn-editor-overlay {:class (when hover-tooltips? "hoverable")}
      [:pre.edn-editor-code
       (if (str/blank? value)
         [:span.edn-placeholder placeholder]
         (if (and hover-tooltips? schema)
           (edn/highlight-edn value
                              {:on-hover #(show-tooltip! % (.-target js/event) schema)
                               :on-leave hide-tooltip!})
           (edn/highlight-edn value)))]]
     ;; Tooltip for hover docs
     (when hover-tooltips?
       (tooltip-view))]))

(defn editor-autocomplete
  "Render autocomplete dropdown for an editor. Call this OUTSIDE the edn-editor
   container to avoid overflow clipping issues.

   Props:
   - :editor-id - The editor ID to target
   - :autocomplete - Current autocomplete state
   - :on-change - Callback when text changes
   - :on-autocomplete-hide - Callback to hide autocomplete
   - :on-autocomplete-select - Callback to update selected index"
  [{:keys [editor-id autocomplete on-change on-autocomplete-hide on-autocomplete-select]}]
  (when autocomplete
    (autocomplete-dropdown
     {:autocomplete autocomplete
      :on-select (fn [idx]
                   (let [textarea (js/document.querySelector (str "#" editor-id))
                         updated-ac (assoc autocomplete :selected idx)
                         {:keys [text cursor-pos]} (edn/apply-completion
                                                    (.-value textarea)
                                                    (.-selectionStart textarea)
                                                    updated-ac)]
                     (set! (.-value textarea) text)
                     (set-cursor-position! textarea cursor-pos)
                     (.focus textarea)
                     (when on-change (on-change text))
                     (when on-autocomplete-hide (on-autocomplete-hide))))
      :on-hover (fn [idx]
                  (when on-autocomplete-select
                    (on-autocomplete-select idx)))})))

