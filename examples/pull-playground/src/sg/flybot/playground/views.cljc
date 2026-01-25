(ns sg.flybot.playground.views
  "UI views - pure functions returning hiccup.

   Views emit events by calling (dispatch! :event) or (dispatch! [:event arg])."
  (:require [sg.flybot.playground.examples :as examples]))

;;=============================================================================
;; Helpers
;;=============================================================================

(defn- format-result
  "Format a Clojure value for display"
  [v]
  (if (nil? v)
    "nil"
    (pr-str v)))

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
  (let [mode (:mode state)]
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
  (let [{:keys [schema schema-loading? schema-error]} state]
    [:div.schema-section
     [:div.schema-header
      [:label "Server Schema"]
      [:button.fetch-schema-btn {:on {:click #(dispatch! :fetch-schema)}
                                 :disabled schema-loading?}
       (if schema-loading? "Loading..." "Fetch")]]
     [:div.schema-content
      (cond
        schema-error
        [:div.schema-error schema-error]

        schema
        [:pre.schema-value (format-result schema)]

        :else
        [:div.schema-empty "Click Fetch to load schema"])]]))

(defn editor-panel [state dispatch!]
  (let [{:keys [mode pattern-text data-text server-url loading?]} state]
    [:div.panel.editor-panel
     [:div.panel-header
      [:h2 "Editor"]]
     [:div.panel-content
      [:div.editor-section
       [:label "Pattern"]
       [:textarea {:value pattern-text
                   :placeholder "Enter a pull pattern, e.g. {:name ?n}"
                   :on {:input #(dispatch! [:update-pattern (.. % -target -value)])}}]]
      (if (= mode :local)
        [:div.editor-section
         [:label "Data (EDN)"]
         [:textarea {:value data-text
                     :placeholder "Enter EDN data to match against"
                     :on {:input #(dispatch! [:update-data (.. % -target -value)])}}]]
        [:div.remote-sections
         [:div.editor-section
          [:label "Server URL"]
          [:input {:type "text"
                   :value server-url
                   :placeholder "http://localhost:8081/api"
                   :on {:input #(dispatch! [:update-server-url (.. % -target -value)])}}]]
         (schema-display state dispatch!)])
      [:button.execute-btn {:on {:click #(dispatch! :execute)}
                            :disabled loading?}
       (if loading? "Executing..." "Execute")]]]))

(defn results-panel [state _dispatch!]
  (let [{:keys [result error loading?]} state]
    [:div.panel.results-panel
     [:div.panel-header
      [:h2 "Results"]]
     [:div.panel-content
      (cond
        loading?
        [:div.loading "Executing pattern..."]

        error
        [:div.result-section
         [:h3 "Error"]
         [:div.result-value.error error]]

        result
        [:div.result-section
         [:h3 "Variable Bindings"]
         [:div.result-value.success (format-result result)]]

        :else
        [:div.result-value.empty "Enter a pattern and data, then click Execute"])]]))

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
  [:div.app-container
   (site-header state dispatch!)
   [:div.main-content
    (editor-panel state dispatch!)
    (results-panel state dispatch!)
    (examples-panel state dispatch!)]])

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
