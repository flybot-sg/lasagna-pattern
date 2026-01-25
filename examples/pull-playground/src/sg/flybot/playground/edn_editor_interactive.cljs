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

;;=============================================================================
;; Editor Component
;;=============================================================================

(defn edn-editor
  "Interactive EDN editor with syntax highlighting and parinfer.

   Props:
   - :value - Current text value
   - :on-change - Callback (fn [new-value]) when text changes
   - :placeholder - Placeholder text when empty
   - :class - Additional CSS class
   - :parinfer-mode - :indent, :paren, :smart, or nil to disable (default :indent)
   - :read-only? - If true, editor is read-only"
  [{:keys [value on-change placeholder class parinfer-mode read-only?]
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
                             cursor (get-cursor-info textarea)
                             ;; Apply parinfer if enabled
                             processed (if parinfer-mode
                                         (apply-parinfer new-text parinfer-mode cursor)
                                         new-text)]
                         ;; Only update if parinfer changed something
                         (when (and parinfer-mode (not= new-text processed))
                           (set! (.-value textarea) processed))
                         (on-change (or processed new-text)))))
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
         (edn/highlight-edn value))]]]))

(defn schema-viewer
  "Read-only schema viewer with syntax highlighting.
   Optimized for displaying server schemas."
  [{:keys [value placeholder class loading? error]}]
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
    [:div.schema-viewer {:class class}
     [:pre.schema-code
      (edn/highlight-edn value)]]))
