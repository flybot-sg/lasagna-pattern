(ns jibbit
  "Jibbit customizations for ECR deployment.

   - entry-point: Custom entrypoint allowing ${JAVA_OPTS} expansion at runtime
   - tagger: Generate ECR image URI from CI environment variables

   Note: This namespace is only loaded during container builds (clj -T:jib build).
   The jibbit.core dependency is provided by the :jib alias."
  (:require
   [clojure.string :as str]
   [jibbit.core :as jibbit]))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn entry-point
  "Custom entry-point to expand ${JAVA_OPTS} at runtime.
   Uses /bin/sh -c wrapper for shell variable expansion."
  [{:keys [basis main working-dir]}]
  ["/bin/sh" "-c"
   (str/join " "
             ["java ${JAVA_OPTS}"
              "-Dclojure.main.report=stderr"
              "-Dfile.encoding=UTF-8"
              "-cp" (jibbit/container-cp basis working-dir)
              "clojure.main"
              "-m" (pr-str main)])])

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn tagger
  "Generate ECR image URI from CI environment variables.
   Reads ECR_REGISTRY, ECR_REPOSITORY, and GITHUB_SHA_SHORT.
   Returns: <registry>/<repo>:<sha>
   Throws if required env vars are missing."
  [_]
  (if-let [registry (System/getenv "ECR_REGISTRY")]
    (if-let [repo (System/getenv "ECR_REPOSITORY")]
      (if-let [sha (System/getenv "GITHUB_SHA_SHORT")]
        (str registry "/" repo ":" sha)
        (throw (ex-info "Missing GITHUB_SHA_SHORT env var" {})))
      (throw (ex-info "Missing ECR_REPOSITORY env var" {})))
    (throw (ex-info "Missing ECR_REGISTRY env var" {}))))
