(ns sg.flybot.playground.ui.core.views
  "UI views — defalias components with namespaced props.

   Components receive data via ::keys and dispatch effects directly."
  (:require [sg.flybot.playground.ui.core.views.examples :as examples]
            [sg.flybot.playground.ui.core.views.edn-editor-interactive.editor :as edn]
            [sg.flybot.playground.ui.core.state :as state]
            [clojure.string :as str]
            #?(:cljs [sg.flybot.playground.ui.core.views.edn-editor-interactive :as edn-i])
            #?(:cljs [replicant.alias :refer [defalias]])))

;;=============================================================================
;; Helpers
;;=============================================================================

(defn- format-result [v]
  (if (nil? v) "nil" (pr-str v)))

(defn- format-result-pretty [v]
  (str/trim (edn/pretty-str v)))

;;=============================================================================
;; Icons
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

;;=============================================================================
;; Components
;;=============================================================================

#?(:cljs
   (defalias site-header
     [{::keys [mode dispatch!]}]
     [:header.site-header
      [:h1 "Pull Pattern Playground"]
      [:div.header-right
       [:div.mode-toggle
        [:button {:class (when (= mode :sandbox) "active")
                  :on {:click #(dispatch! {:db (fn [db] (state/set-mode db :sandbox))})}}
         "Sandbox"]
        [:button {:class (when (= mode :remote) "active")
                  :on {:click #(dispatch! {:db (fn [db] (state/set-mode db :remote))})}}
         "Remote"]]
       [:button.theme-toggle {:title "Toggle theme"
                              :on {:click #(js/sg.flybot.playground.ui.core.toggle_theme_BANG_)}}
        [:span.show-light (moon-icon)]
        [:span.show-dark (sun-icon)]]]]))

#?(:clj
   (defn site-header [{::keys [mode dispatch!]}]
     [:header.site-header
      [:h1 "Pull Pattern Playground"]]))

;;-----------------------------------------------------------------------------
;; Data Panel
;;-----------------------------------------------------------------------------

(defn- data-display
  "Read-only display of sample data with :data/:schema toggle and optional reset."
  [{::keys [sandbox-data sandbox-schema data-view mode dispatch!]}]
  [:div.sandbox-data-section
   [:div.sandbox-header
    [:div.data-view-toggle
     [:button {:class (when (= data-view :data) "active")
               :on {:click #(dispatch! {:db (fn [db] (assoc db :data-view :data))})}}
      "Data"]
     [:button {:class (when (= data-view :schema) "active")
               :on {:click #(dispatch! {:db (fn [db] (assoc db :data-view :schema))})}}
      "Schema"]]
    (when (= mode :sandbox)
      [:button.reset-btn {:on {:click #(dispatch! {:exec {:type :sandbox-reset}})}}
       "Reset"])]
   [:div.sandbox-content
    (let [text (format-result-pretty (if (= data-view :schema) sandbox-schema sandbox-data))]
      (edn/edn-display {:value text :placeholder "No data"}))]])

#?(:cljs
   (defalias data-panel
     [{::keys [db dispatch!]}]
     (let [{:keys [mode server-url sandbox-data sandbox-schema data-view]} db]
       [:div.panel.data-panel
        [:div.panel-header
         [:h2 "Data"]]
        [:div.panel-content
         (when (= mode :remote)
           [:div.remote-sections
            [:div.remote-info
             "Remote mode connects to a running Pull Pattern server. "
             "Clone the repository and run the demo server locally to try it out."]
            [:div.editor-section.url-section
             [:label "Server URL"]
             [:input {:type "text"
                      :value server-url
                      :placeholder "http://localhost:8081/api"
                      :on {:input #(dispatch! {:db (fn [db] (assoc db :server-url (.. % -target -value)))})}}]]])
         (data-display
          {::sandbox-data sandbox-data
           ::sandbox-schema sandbox-schema
           ::data-view data-view
           ::mode mode
           ::dispatch! dispatch!})]]))

   :clj
   (defn data-panel [{::keys [db dispatch!]}]
     [:div.panel.data-panel
      [:div.panel-header
       [:h2 "Data"]]
      [:div.panel-content
       [:div "Sample data"]]]))

;;-----------------------------------------------------------------------------
;; Pattern + Results Panel
;;-----------------------------------------------------------------------------

(def ^:private pattern-editor-id "pattern-editor")

(defn- execute-effect
  "Build :exec effect based on mode."
  [db]
  (let [{:keys [mode pattern-text server-url]} db]
    (case mode
      :sandbox {:type :sandbox :pattern pattern-text}
      :remote  {:type :remote :pattern pattern-text :url server-url})))

#?(:cljs
   (defalias pattern-results-panel
     [{::keys [db dispatch!]}]
     (let [{:keys [mode pattern-text loading? schema sandbox-schema autocomplete result error]} db
           active-schema (case mode
                           :remote schema
                           :sandbox sandbox-schema
                           nil)]
       [:div.panel.pattern-results-panel
        ;; Pattern section
        [:div.pattern-section
         [:div.section-header
          [:label "Pattern"]
          [:button.execute-btn
           {:on {:click #(dispatch! {:db state/set-loading
                                     :exec (execute-effect db)})}
            :disabled loading?}
           (if loading? "Executing..." "Execute")]]
         [:div.pattern-editor
          (edn-i/edn-editor
           {:value pattern-text
            :placeholder "Enter a pull pattern, e.g. {:name ?n}"
            :on-change #(dispatch! {:db (fn [db] (assoc db :pattern-text %))})
            :parinfer-mode :indent
            :editor-id pattern-editor-id
            :schema (when (#{:remote :sandbox} mode) active-schema)
            :hover-tooltips? (and (#{:remote :sandbox} mode) (some? active-schema))
            :autocomplete (when (#{:remote :sandbox} mode) autocomplete)
            :on-autocomplete #(dispatch! {:db (fn [db] (state/show-autocomplete db %))})
            :on-autocomplete-hide #(dispatch! {:db state/hide-autocomplete})
            :on-autocomplete-select #(dispatch! {:db (fn [db] (state/select-autocomplete db %))})
            :on-autocomplete-move #(dispatch! {:db (fn [db] (state/move-autocomplete-selection db %))})})]
         (when (and (#{:remote :sandbox} mode) autocomplete)
           (edn-i/editor-autocomplete
            {:editor-id pattern-editor-id
             :autocomplete autocomplete
             :on-change #(dispatch! {:db (fn [db] (assoc db :pattern-text %))})
             :on-autocomplete-hide #(dispatch! {:db state/hide-autocomplete})
             :on-autocomplete-select #(dispatch! {:db (fn [db] (state/select-autocomplete db %))})}))]
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

   :clj
   (defn pattern-results-panel [{::keys [db dispatch!]}]
     (let [{:keys [pattern-text result error]} db]
       [:div.panel.pattern-results-panel
        [:div.pattern-section
         [:div.section-header
          [:label "Pattern"]
          [:button.execute-btn "Execute"]]
         [:div.pattern-editor
          [:textarea {:value pattern-text
                      :placeholder "Enter a pull pattern, e.g. {:name ?n}"}]]]
        [:div.results-section
         [:div.section-header [:label "Results"]]
         [:div.results-content
          (cond
            error [:div.result-value.error error]
            result [:div.result-value.success (format-result-pretty result)]
            :else [:div.result-value.empty "Enter a pattern and data, then click Execute"])]]])))

;;-----------------------------------------------------------------------------
;; Examples Panel
;;-----------------------------------------------------------------------------

#?(:cljs
   (defalias examples-panel
     [{::keys [selected-example dispatch!]}]
     [:div.panel.examples-panel
      [:div.panel-header
       [:h2 "Examples"]]
      [:div.panel-content
       [:ul.example-list
        (for [[idx example] (map-indexed vector examples/examples)]
          [:li {:replicant/key idx
                :class (when (= idx selected-example) "active")
                :title (:description example)
                :on {:click #(dispatch!
                              {:db (fn [db]
                                     (-> db
                                         (assoc :pattern-text (:pattern example)
                                                :selected-example idx)
                                         state/clear-result))})}}
           (:name example)])]
       [:div.syntax-reference
        [:h3 "Syntax Reference"]
        [:div.syntax-list
         (for [{:keys [syntax description]} examples/syntax-reference]
           [:div.syntax-item {:replicant/key syntax}
            [:code syntax]
            [:span description]])]]]])

   :clj
   (defn examples-panel [{::keys [selected-example dispatch!]}]
     [:div.panel.examples-panel
      [:div.panel-header [:h2 "Examples"]]
      [:div.panel-content
       [:ul.example-list
        (for [[idx example] (map-indexed vector examples/examples)]
          [:li {:replicant/key idx
                :class (when (= idx selected-example) "active")}
           (:name example)])]]]))

;;=============================================================================
;; App Root
;;=============================================================================

(defn app-view [{::keys [db dispatch!]}]
  (let [{:keys [mode selected-example]} db]
    [:div.app-container
     #?(:cljs [::site-header {::mode mode ::dispatch! dispatch!}]
        :clj  (site-header {::mode mode ::dispatch! dispatch!}))
     [:div.main-content.with-sidebar
      #?(:cljs [::data-panel {::db db ::dispatch! dispatch!}]
         :clj  (data-panel {::db db ::dispatch! dispatch!}))
      #?(:cljs [::pattern-results-panel {::db db ::dispatch! dispatch!}]
         :clj  (pattern-results-panel {::db db ::dispatch! dispatch!}))
      #?(:cljs [::examples-panel {::selected-example selected-example
                                  ::dispatch! dispatch!}]
         :clj  (examples-panel {::selected-example selected-example
                                ::dispatch! dispatch!}))]]))

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
  (first (app-view {::db {:mode :sandbox :pattern-text ""}
                    ::dispatch! identity}))
  ;=> :div.app-container

  ;; execute-effect builds correct effect for each mode
  (:type (execute-effect {:mode :sandbox :pattern-text "p"})) ;=> :sandbox
  (:type (execute-effect {:mode :remote :pattern-text "p" :server-url "u"}))) ;=> :remote)
