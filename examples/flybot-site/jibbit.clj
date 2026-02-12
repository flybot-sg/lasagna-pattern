(ns jibbit
  "Jibbit customizations for ECR deployment.

   Custom entrypoint allowing ${JAVA_OPTS} expansion at runtime.

   Note: This namespace is only loaded during container builds (clj -T:jib build).
   The jibbit.core dependency is provided by the :jib alias."
  (:require
   [clojure.string :as str]
   [jibbit.core :as jibbit]))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn entry-point
  "Custom entry-point to expand ${JAVA_OPTS} at runtime.
   Uses /bin/sh -c wrapper for shell variable expansion.
   When AOT is enabled, uses java -jar with direct linking and tiered compilation."
  [{:keys [basis aot jar-name main working-dir]}]
  ["/bin/sh" "-c"
   (str/join " "
             (concat ["java ${JAVA_OPTS}"
                      "-Dclojure.main.report=stderr"
                      "-Dfile.encoding=UTF-8"]
                     (if aot
                       ["-Dclojure.compiler.direct-linking=true"
                        "-XX:+TieredCompilation"
                        "-XX:TieredStopAtLevel=1"
                        "-jar" jar-name]
                       ["-cp" (jibbit/container-cp basis working-dir)
                        "clojure.main" "-m" (pr-str main)])))])
