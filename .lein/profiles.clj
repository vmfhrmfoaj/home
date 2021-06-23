{:dev {:test-refresh {:changes-only true
                      :timeout-in-sec 5
                      :notify-command ["notify-send" "--hint" "int:transient:1" "Test"]}}
 :repl {:auto-refresh {:notify-command ["notify-send" "--hint" "int:transient:1" "Reload"]}
        :jvm-opts ["-XX:MaxGCPauseMillis=5" "-Xmx2g" "-server"]
        :middleware [dev-helper.plugin/middleware]}
 :user {:mirrors {"central" {:name "Maven (mirror)"
                             :url "https://localhost:4430/repository/maven-public/"}
                  "clojars" {:name "Clojars (mirror)"
                             :url "https://localhost:4430/repository/clojars-group/"
                             :repo-manager true}}
        :certificates ["/opt/localhost/nexus.crt"]
        :deploy-repositories [["snapshots" {:url "https://localhost:4430/repository/clojars-snapshots/" :sign-releases false}]
                              ["releases"  {:url "https://localhost:4430/repository/clojars-releases/"  :sign-releases false}]]
        :plugins [[lein-cljfmt "0.7.0"]
                  [lein-pprint "1.3.2"]
                  [vmfhrmfoaj/my-lein-templates "1.0.2"]
                  [vmfhrmfoaj/my-lein-utils  "1.0.5"]]
        :repl-options {:init (do
                               (set! *print-level* 8)
                               (set! *print-length* 30))}}}
