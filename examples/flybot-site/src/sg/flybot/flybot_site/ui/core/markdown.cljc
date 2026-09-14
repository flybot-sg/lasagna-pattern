(ns sg.flybot.flybot-site.ui.core.markdown
  "Runtime markdown rendering: marked + highlight.js, with ```mermaid fences
   drawn by a lazily loaded, self-hosted mermaid."
  (:require [clojure.string :as str]
            [sg.flybot.flybot-site.ui.core.db :as db]
            #?(:cljs ["marked" :refer [Marked]])
            #?(:cljs ["highlight.js/lib/core" :as hljs])
            #?(:cljs ["highlight.js/lib/languages/clojure" :as hljs-clojure])
            #?(:cljs ["highlight.js/lib/languages/javascript" :as hljs-js])
            #?(:cljs ["highlight.js/lib/languages/typescript" :as hljs-ts])
            #?(:cljs ["highlight.js/lib/languages/python" :as hljs-python])
            #?(:cljs ["highlight.js/lib/languages/java" :as hljs-java])
            #?(:cljs ["highlight.js/lib/languages/go" :as hljs-go])
            #?(:cljs ["highlight.js/lib/languages/rust" :as hljs-rust])
            #?(:cljs ["highlight.js/lib/languages/ruby" :as hljs-ruby])
            #?(:cljs ["highlight.js/lib/languages/php" :as hljs-php])
            #?(:cljs ["highlight.js/lib/languages/c" :as hljs-c])
            #?(:cljs ["highlight.js/lib/languages/cpp" :as hljs-cpp])
            #?(:cljs ["highlight.js/lib/languages/csharp" :as hljs-csharp])
            #?(:cljs ["highlight.js/lib/languages/kotlin" :as hljs-kotlin])
            #?(:cljs ["highlight.js/lib/languages/swift" :as hljs-swift])
            #?(:cljs ["highlight.js/lib/languages/sql" :as hljs-sql])
            #?(:cljs ["highlight.js/lib/languages/bash" :as hljs-bash])
            #?(:cljs ["highlight.js/lib/languages/json" :as hljs-json])
            #?(:cljs ["highlight.js/lib/languages/xml" :as hljs-xml])
            #?(:cljs ["highlight.js/lib/languages/yaml" :as hljs-yaml])
            #?(:cljs ["highlight.js/lib/languages/css" :as hljs-css])
            #?(:cljs ["highlight.js/lib/languages/markdown" :as hljs-md])
            #?(:cljs ["highlight.js/lib/languages/dockerfile" :as hljs-docker])))

;;=============================================================================
;; highlight.js languages
;;=============================================================================

#?(:cljs
   (do
     (hljs/registerLanguage "clojure" hljs-clojure)
     (hljs/registerLanguage "javascript" hljs-js)
     (hljs/registerLanguage "typescript" hljs-ts)
     (hljs/registerLanguage "python" hljs-python)
     (hljs/registerLanguage "java" hljs-java)
     (hljs/registerLanguage "go" hljs-go)
     (hljs/registerLanguage "rust" hljs-rust)
     (hljs/registerLanguage "ruby" hljs-ruby)
     (hljs/registerLanguage "php" hljs-php)
     (hljs/registerLanguage "c" hljs-c)
     (hljs/registerLanguage "cpp" hljs-cpp)
     (hljs/registerLanguage "csharp" hljs-csharp)
     (hljs/registerLanguage "kotlin" hljs-kotlin)
     (hljs/registerLanguage "swift" hljs-swift)
     (hljs/registerLanguage "sql" hljs-sql)
     (hljs/registerLanguage "bash" hljs-bash)
     (hljs/registerLanguage "shell" hljs-bash)
     (hljs/registerLanguage "json" hljs-json)
     (hljs/registerLanguage "xml" hljs-xml)
     (hljs/registerLanguage "html" hljs-xml)
     (hljs/registerLanguage "yaml" hljs-yaml)
     (hljs/registerLanguage "css" hljs-css)
     (hljs/registerLanguage "markdown" hljs-md)
     (hljs/registerLanguage "dockerfile" hljs-docker)))

;;=============================================================================
;; mermaid (CLJS only)
;;=============================================================================

(defn- escape-attr
  "Escape s for a double-quoted HTML attribute value."
  [s]
  (-> s
      (str/replace "&" "&amp;")
      (str/replace "\"" "&quot;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

^:rct/test
(comment
  (escape-attr "A[\"start\"] --> B") ;=> "A[&quot;start&quot;] --&gt; B"
  ;; & first, so its entities are not escaped again
  (escape-attr "a & b<br/>") ;=> "a &amp; b&lt;br/&gt;"
  )

#?(:cljs
   (def ^:private mermaid-url "/vendor/mermaid.min.js"))

#?(:cljs
   (defonce ^:private mermaid-promise (atom nil)))

#?(:cljs
   (defn- load-mermaid!
     "Load the mermaid bundle once; resolves to window.mermaid."
     []
     (or @mermaid-promise
         (reset! mermaid-promise
                 (js/Promise.
                  (fn [resolve reject]
                    (if-let [m (.-mermaid js/window)]
                      (resolve m)
                      (let [s (.createElement js/document "script")]
                        (set! (.-src s) mermaid-url)
                        (set! (.-async s) true)
                        (set! (.-onload s) #(resolve (.-mermaid js/window)))
                        (set! (.-onerror s) reject)
                        (.appendChild (.-head js/document) s)))))))))

#?(:cljs
   (defonce ^:private diagram-counter (atom 0)))

#?(:cljs
   (defn- show-source!
     "Show a diagram's definition as a code block when it cannot be drawn."
     [node]
     (set! (.-textContent node) (.getAttribute node "data-src"))
     (.add (.-classList node) "mermaid-failed")))

#?(:cljs
   (defn- render-diagram!
     "Draw one pre.mermaid node from its data-src. Never rejects."
     [^js mermaid node]
     (-> (.render mermaid (str "mermaid-" (swap! diagram-counter inc)) (.getAttribute node "data-src"))
         (.then (fn [^js result]
                  (set! (.-innerHTML node) (.-svg result))
                  (.remove (.-classList node) "mermaid-failed")))
         (.catch (fn [e]
                   (js/console.error "mermaid render failed:" e)
                   (show-source! node))))))

#?(:cljs
   (defn- render-diagrams!
     "Draw the diagrams under root, undrawn ones or all when redraw?, one at a
      time with the mermaid theme following body's data-theme."
     [root redraw?]
     (let [nodes (array-seq (.querySelectorAll root (if redraw?
                                                      "pre.mermaid[data-src]"
                                                      "pre.mermaid[data-src]:not([data-processed])")))
           dark? (= "dark" (.getAttribute js/document.body "data-theme"))]
       (when (seq nodes)
         (doseq [n nodes] (.setAttribute n "data-processed" "true"))
         (-> (load-mermaid!)
             (.then (fn [^js mermaid]
                      (.initialize mermaid #js {:startOnLoad false
                                                :securityLevel "strict"
                                                :suppressErrorRendering true
                                                :theme (if dark? "dark" "default")})
                      (reduce (fn [p n] (.then p #(render-diagram! mermaid n)))
                              (js/Promise.resolve)
                              nodes)))
             (.catch (fn [e]
                       (js/console.error "mermaid failed to load:" e)
                       (reset! mermaid-promise nil)
                       (run! show-source! nodes))))))))

#?(:cljs
   (defn- draw-diagrams!
     "Draw the node's undrawn diagrams. A var, not a closure, so unchanged
      hiccup stays `=`."
     [{:keys [replicant/node]}]
     (render-diagrams! node false)))

#?(:cljs
   ;; mermaid bakes the theme into the SVG, so a theme change redraws.
   (defonce ^:private _theme-observer
     (doto (js/MutationObserver. (fn [_ _] (render-diagrams! js/document.body true)))
       (.observe js/document.body #js {:attributes true :attributeFilter #js ["data-theme"]}))))

;;=============================================================================
;; marked instance + entry points
;;=============================================================================

#?(:cljs
   (def ^:private marked-instance
     (let [m (Marked.)]
       (.use m (clj->js
                {:renderer
                 {:code (fn [obj]
                          (let [code (.-text obj)
                                lang (.-lang obj)]
                            (if (= lang "mermaid")
                              (str "<pre class=\"mermaid\" data-src=\"" (escape-attr code) "\"></pre>")
                              (let [highlighted (if (and lang (hljs/getLanguage lang))
                                                  (.-value (hljs/highlight code #js {:language lang}))
                                                  (.-value (hljs/highlightAuto code)))]
                                (str "<pre><code class=\"hljs\">" highlighted "</code></pre>")))))}}))
       m)))

(defn- unescape-markdown [content]
  #?(:clj content
     :cljs (if (string? content)
             (.replace content (js/RegExp. "\\\\([.()\\[\\]])" "g") "$1")
             content)))

(defn render-markdown
  "Markdown to hiccup. CLJ: the text in a pre (for RCT). CLJS: marked +
   highlight.js, with diagrams drawn once the element is in the DOM."
  [content]
  (let [body (-> content db/strip-frontmatter unescape-markdown)]
    #?(:clj [:pre body]
       :cljs (when (seq body)
               [:div {:innerHTML (.parse marked-instance body)
                      :replicant/on-mount draw-diagrams!
                      :replicant/on-update draw-diagrams!}]))))

(defn markdown->text
  "Markdown to plain text, through the DOM so HTML entities decode."
  [s]
  #?(:clj s
     :cljs (let [div (js/document.createElement "div")]
             (set! (.-innerHTML div) (.parse marked-instance s))
             (-> (.-textContent div)
                 (str/replace #"\s+" " ")
                 str/trim))))
