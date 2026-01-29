(ns user
  "REPL development namespace.

   Quick Start:
     (start!)              ; server with dev mode
     (def api (connect))   ; connect client
     (api '{:posts ?all})  ; pull data
     (stop!)               ; stop server"
  (:require
   [sg.flybot.flybot-site.system :as sys]
   [sg.flybot.pullable.remote.client :as client]))

;;=============================================================================
;; Server Lifecycle
;;=============================================================================

(def ^:private dev-config
  "Dev mode config - auto-login as owner."
  {:dev {:mode? true :seed? true}
   :auth {:owner-emails "dev@localhost"}})

(defn start!
  "Start server with dev mode. Pass config to override."
  ([] (start! {}))
  ([config] (sys/start! (merge dev-config config))))

(defn stop! [] (sys/stop!))
(defn restart! [] (sys/restart!))

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
