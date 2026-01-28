(ns user
  "REPL development namespace for blog example.

   ## Quick Start

   1. Start server (with dev mode):
      (start!)

   2. Connect a client:
      (def api (connect))

   3. Pull data:
      (api '{:posts ?posts})
      (api '{:post {:title ?t}} {:post-id 1})

   4. Stop when done:
      (stop!)

   ## Dev Mode

   By default, starts with dev mode enabled (auto-login as dev@localhost).
   The dev user is an owner, so you get full CRUD access.

   To disable dev mode:
      (start! {:dev-mode? false})"
  (:require
   [sg.flybot.flybot-site.server :as server]
   [sg.flybot.pullable.remote.client :as client]))

;;=============================================================================
;; Server Lifecycle
;;=============================================================================

(def ^:private default-dev-opts
  "Default options for dev mode - auto-login as owner."
  {:dev-mode? true
   :owner-emails #{"dev@localhost"}})

(defn start!
  "Start the blog server with dev mode enabled by default.

   Options:
   - :port - HTTP port (default 8080)
   - :seed? - Seed database (default true)
   - :dev-mode? - Enable dev auto-login (default true)
   - :owner-emails - Set of owner emails (default #{\"dev@localhost\"})"
  ([] (start! {}))
  ([opts]
   (server/start! (merge default-dev-opts opts))))

(defn stop!
  "Stop the blog server."
  []
  (server/stop!))

(defn restart!
  "Restart the server."
  []
  (server/restart!))

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
  ;; === START SERVER ===
  ;; Starts with dev mode (auto-login as dev@localhost owner)
  (start!)
  ;; Blog server started on port 8080
  ;;   POST http://localhost:8080/api
  ;;   GET  http://localhost:8080/api/_schema
  ;; WARN DEV MODE: Auto-login as dev@localhost

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
