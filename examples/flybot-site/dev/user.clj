(ns user
  "REPL development namespace for blog example.

   ## Quick Start

   1. Create and start system:
      (def sys (start!))

   2. Connect a client:
      (def api (connect))

   3. Pull data:
      (api '{:posts ?posts})
      (api '{:post {:title ?t}} {:post-id 1})

   4. Stop when done:
      (stop!)"
  (:require
   [sg.flybot.flybot-site.system :as system]
   [sg.flybot.pullable.remote.client :as client]
   [robertluo.fun-map :refer [halt!]]))

;;=============================================================================
;; System Lifecycle
;;=============================================================================

(defonce ^:private sys (atom nil))

(declare stop!)

(defn start!
  "Create and start the blog system.

   Options:
   - :port - HTTP port (default 8080)
   - :seed? - Seed database (default true)"
  ([] (start! {}))
  ([opts]
   (when @sys (stop!))
   (let [s (system/make-system opts)]
     ;; Touch :server to start everything
     (:server s)
     (reset! sys s))))

(defn stop!
  "Stop the blog system."
  []
  (when-let [s @sys]
    (halt! s)
    (reset! sys nil)))

(defn restart!
  "Restart the system."
  []
  (stop!)
  (start!))

;;=============================================================================
;; Client
;;=============================================================================

(defn connect
  "Connect to the blog API.

   Returns a function you can call with patterns:
     (def api (connect))
     (api '{:posts ?posts})
     (api '{:post {:title ?t}} {:post-id 1})"
  ([]
   (connect "http://localhost:8080/api"))
  ([url]
   (client/connect url)))

(defn schema
  "Get the API schema."
  [api]
  (client/schema api))

;;=============================================================================
;; Convenience Helpers
;;=============================================================================

(defn vars
  "Extract vars from a pull response."
  [response]
  (:vars response))

(defn data
  "Extract data from a pull response."
  [response]
  (:data response))

;;=============================================================================
;; Demo Session
;;=============================================================================

(comment
  ;; === START SYSTEM ===
  ;; Creates db, app, and server as one unit
  (start!)
  ;; Blog server started on port 8080
  ;;   POST http://localhost:8080/api
  ;;   GET  http://localhost:8080/api/_schema

  ;; Inspect the system
  @sys
  ;; => {:port 8080, :db #<Atom>, :api-fn #<Fn>, :app #<Fn>, :server {...}}

  ;; === CONNECT CLIENT ===
  (def api (connect))

  ;; === INTROSPECT ===
  (schema api)
  ;; => {:post {...} :posts [...] :post-count :number ...}

  ;; === QUERIES ===

  ;; List all posts
  (api '{:posts ?posts})

  ;; Extract just the posts from vars
  (-> (api '{:posts ?posts}) vars (get 'posts))

  ;; Get titles
  (->> (api '{:posts ?posts}) vars (get 'posts) (map :title))

  ;; Single post by ID
  (api '{:post {:id ?id :title ?t :content ?c}}
       {:post-id 1})

  ;; Count posts
  (api '{:post-count ?n})

  ;; Filter by author
  (api '{:posts ?posts} {:author "Alice"})

  ;; Filter by tag
  (api '{:posts ?posts} {:tag "clojure"})

  ;; === MUTATIONS ===

  ;; Create a post
  (api '{:create-post {:id ?id :title ?t}}
       {:create-post {:title "New Post"
                      :content "Created via remote API!"
                      :author "Developer"
                      :tags ["remote" "demo"]}})

  ;; Verify creation
  (api '{:post-count ?n})

  ;; Update a post
  (api '{:update-post {:title ?t :updated-at ?when}}
       {:update-post {:id 1 :title "Updated Title"}})

  ;; Delete a post (use the id from create above)
  (api '{:delete-post ?deleted} {:post-id 4})

  ;; === COMBINED QUERIES ===

  ;; Multiple queries in one request
  (api '{:posts ?posts
         :post-count ?count})

  ;; === STOP ===
  ;; Halts server and clears db
  (stop!))
