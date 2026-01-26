(ns sg.flybot.playground.views
  "UI views - pure functions returning hiccup.

   Views emit events by calling (dispatch! :event) or (dispatch! [:event arg])."
  (:require [sg.flybot.playground.examples :as examples]
            [sg.flybot.playground.edn-editor :as edn]
            [clojure.string :as str]
            #?(:cljs [sg.flybot.playground.edn-editor-interactive :as edn-i])))

;;=============================================================================
;; Helpers
;;=============================================================================

(defn- format-result
  "Format a Clojure value for display (single line)"
  [v]
  (if (nil? v)
    "nil"
    (pr-str v)))

(defn- format-result-pretty
  "Format a Clojure value with pretty-printing"
  [v]
  (str/trim (edn/pretty-str v)))

;;=============================================================================
;; Components
;;=============================================================================

(defn- sun-icon []
  [:svg {:width "16" :height "16" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:circle {:cx "12" :cy "12" :r "5"}]
   [:line {:x1 "12" :y1 "1" :x2 "12" :y2 "3"}]
   [:line {:x1 "12" :y1 "21" :x2 "12" :y2 "23"}]
   [:line {:x1 "4.22" :y1 "4.22" :x2 "5.64" :y2 "5.64"}]
   [:line {:x1 "18.36" :y1 "18.36" :x2 "19.78" :y2 "19.78"}]
   [:line {:x1 "1" :y1 "12" :x2 "3" :y2 "12"}]
   [:line {:x1 "21" :y1 "12" :x2 "23" :y2 "12"}]
   [:line {:x1 "4.22" :y1 "19.78" :x2 "5.64" :y2 "18.36"}]
   [:line {:x1 "18.36" :y1 "5.64" :x2 "19.78" :y2 "4.22"}]])

(defn- moon-icon []
  [:svg {:width "16" :height "16" :viewBox "0 0 24 24" :fill "none" :stroke "currentColor" :stroke-width "2"}
   [:path {:d "M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z"}]])

(defn site-header [state dispatch!]
  (let [{:keys [mode]} state]
    [:header.site-header
     [:h1 "Pull Pattern Playground"]
     [:div.header-right
      [:div.mode-toggle
       [:button {:class (when (= mode :local) "active")
                 :on {:click #(dispatch! [:set-mode :local])}}
        "Local"]
       [:button {:class (when (= mode :remote) "active")
                 :on {:click #(dispatch! [:set-mode :remote])}}
        "Remote"]]
      [:button.theme-toggle {:title "Toggle theme"
                             :on {:click #?(:clj identity
                                            :cljs #(js/sg.flybot.playground.core.toggle_theme_BANG_))}}
       [:span.show-light (moon-icon)]
       [:span.show-dark (sun-icon)]]]]))

(defn- schema-display [state dispatch!]
  (let [{:keys [schema schema-loading? schema-error sample-data schema-view-mode]} state]
    [:div.schema-section
     [:div.schema-header
      [:label "Server Schema"]
      [:button.fetch-schema-btn {:on {:click #(dispatch! :fetch-schema)}
                                 :disabled schema-loading?}
       (if schema-loading? "Loading..." "Fetch")]]
     #?(:cljs (edn-i/schema-viewer
               {:schema schema
                :sample-data sample-data
                :view-mode schema-view-mode
                :on-mode-change #(dispatch! [:set-schema-view-mode %])
                :loading? schema-loading?
                :error schema-error
                :placeholder "Click Fetch to load schema"})
        :clj [:div.schema-content
              (cond
                schema-error [:div.schema-error schema-error]
                schema [:pre.schema-value (format-result-pretty schema)]
                :else [:div.schema-empty "Click Fetch to load schema"])])]))

(defn data-panel
  "Left panel: Data (local) or Schema (remote)"
  [state dispatch!]
  (let [{:keys [mode data-text server-url]} state]
    [:div.panel.data-panel
     [:div.panel-header
      [:h2 (if (= mode :local) "Data" "Schema")]]
     [:div.panel-content
      (if (= mode :local)
        [:div.editor-section
         #?(:cljs (edn-i/edn-editor
                   {:value data-text
                    :placeholder "Enter EDN data to match against"
                    :on-change #(dispatch! [:update-data %])
                    :parinfer-mode :indent})
            :clj [:textarea {:value data-text
                             :placeholder "Enter EDN data to match against"}])]
        [:div.remote-sections
         [:div.editor-section.url-section
          [:label "Server URL"]
          [:input {:type "text"
                   :value server-url
                   :placeholder "http://localhost:8081/api"
                   :on {:input #(dispatch! [:update-server-url (.. % -target -value)])}}]]
         (schema-display state dispatch!)])]]))

(def ^:private pattern-editor-id "pattern-editor")

(defn pattern-results-panel
  "Right panel: Pattern editor + Results stacked vertically"
  [state dispatch!]
  (let [{:keys [mode pattern-text loading? schema autocomplete result error]} state]
    [:div.panel.pattern-results-panel
     ;; Pattern section
     [:div.pattern-section
      [:div.section-header
       [:label "Pattern"]
       [:button.execute-btn {:on {:click #(dispatch! :execute)}
                             :disabled loading?}
        (if loading? "Executing..." "Execute")]]
      [:div.pattern-editor
       #?(:cljs (edn-i/edn-editor
                 {:value pattern-text
                  :placeholder "Enter a pull pattern, e.g. {:name ?n}"
                  :on-change #(dispatch! [:update-pattern %])
                  :parinfer-mode :indent
                  :editor-id pattern-editor-id
                  ;; Pass schema for autocomplete in remote mode
                  :schema (when (= mode :remote) schema)
                  ;; Autocomplete state and callbacks (dropdown rendered separately below)
                  :autocomplete (when (= mode :remote) autocomplete)
                  :on-autocomplete #(dispatch! [:show-autocomplete %])
                  :on-autocomplete-hide #(dispatch! :hide-autocomplete)
                  :on-autocomplete-select #(dispatch! [:select-autocomplete %])
                  :on-autocomplete-move #(dispatch! [:move-autocomplete %])})
          :clj [:textarea {:value pattern-text
                           :placeholder "Enter a pull pattern, e.g. {:name ?n}"}])]
      ;; Autocomplete dropdown - rendered outside editor to avoid overflow clipping
      #?(:cljs (when (and (= mode :remote) autocomplete)
                 (edn-i/editor-autocomplete
                  {:editor-id pattern-editor-id
                   :autocomplete autocomplete
                   :on-change #(dispatch! [:update-pattern %])
                   :on-autocomplete-hide #(dispatch! :hide-autocomplete)
                   :on-autocomplete-select #(dispatch! [:select-autocomplete %])}))
         :clj nil)]
     ;; Results section
     [:div.results-section
      [:div.section-header
       [:label "Results"]]
      [:div.results-content
       (cond
         loading?
         [:div.loading "Executing pattern..."]

         error
         [:div.result-value.error error]

         result
         [:div.result-value.success
          (edn/highlight-edn (format-result-pretty result))]

         :else
         [:div.result-value.empty "Enter a pattern and data, then click Execute"])]]]))

(defn examples-panel [state dispatch!]
  (let [selected (:selected-example state)]
    [:div.panel.examples-panel
     [:div.panel-header
      [:h2 "Examples"]]
     [:div.panel-content
      [:ul.example-list
       (for [[idx example] (map-indexed vector examples/examples)]
         [:li {:replicant/key idx
               :class (when (= idx selected) "active")
               :title (:description example)
               :on {:click #(do (dispatch! [:select-example example])
                                (dispatch! [:set-selected-example idx]))}}
          (:name example)])]
      [:div.syntax-reference
       [:h3 "Syntax Reference"]
       [:div.syntax-list
        (for [{:keys [syntax description]} examples/syntax-reference]
          [:div.syntax-item {:replicant/key syntax}
           [:code syntax]
           [:span description]])]]]]))

(defn app-view [state dispatch!]
  (let [{:keys [mode]} state]
    [:div.app-container
     (site-header state dispatch!)
     [:div.main-content {:class (when (= mode :local) "with-sidebar")}
      (data-panel state dispatch!)
      (pattern-results-panel state dispatch!)
      (when (= mode :local)
        (examples-panel state dispatch!))]]))

;;=============================================================================
;; Tests
;;=============================================================================

^:rct/test
(comment
  ;; format-result handles nil
  (format-result nil) ;=> "nil"

  ;; format-result handles maps
  (format-result {:a 1}) ;=> "{:a 1}"

  ;; app-view returns container
  (first (app-view {:mode :local :pattern-text "" :data-text ""} identity))) ;=> :div.app-container)
