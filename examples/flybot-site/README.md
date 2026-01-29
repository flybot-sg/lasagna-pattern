# Flybot Site

Migration of flybot.sg company site. Internal portal with blog-based content.

## Quick Start

```bash
# Build frontend
npm install && npm run release

# Start server
clj -M:run
```

Open http://localhost:8080.

## REPL Development

```bash
clj -M:dev
```

```clojure
(start!)  ; server with dev mode (auto-login as owner)

;; Connect client
(def api (connect))
(api '{:posts ?all})
(api '{:posts {{:post/id 1} ?post}})

(stop!)
```

## fun-map Lifecycle

The system uses [fun-map](https://github.com/robertluo/fun-map) `life-cycle-map` for dependency injection:

```clojure
(require '[sg.flybot.flybot-site.system :as sys]
         '[robertluo.fun-map :refer [halt!]])

;; Create system (components not started yet)
(def sys (sys/make-system {:server {:port 8080}
                           :auth {:owner-emails "alice@example.com"}}))

;; Touch ::http-server to start the full chain
(::sys/http-server sys)

;; Stop
(halt! sys)
```

## Config

Pass config with `:*` keys:

```clojure
{:server {:port 8080 :base-url "http://localhost:8080"}
 :db {:backend :mem}  ; or :file, :s3
 :auth {:owner-emails "alice@example.com"
            :google-client-id "..."
            :google-client-secret "..."}
 :session {:secret "32-hex-chars" :timeout 43200}
 :dev {:mode? true :seed? true}}
```

Or use environment variables (see `config-from-env` in system.clj).

## Noun-Only CRUD

No verbs - CRUD via pattern syntax on collections:

| Pattern | Operation |
|---------|-----------|
| `{:posts ?all}` | LIST |
| `{:posts {{:post/id 3} ?post}}` | READ |
| `{:posts {nil {:post/title "..."}}}` | CREATE |
| `{:posts {{:post/id 3} {:post/title "..."}}}` | UPDATE |
| `{:posts {{:post/id 3} nil}}` | DELETE |

## Testing

```bash
clj -X:dev:test
```
