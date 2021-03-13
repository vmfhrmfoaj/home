{:repl {:jvm-opts ["-Xverify:none" "-XX:TieredStopAtLevel=4" "-XX:MaxGCPauseMillis=5" "-Xmx1g" "-server"]
        :middleware [dev-helper.plugin/middleware]}
 :user {:jvm-opts ["-Xverify:none" "-XX:TieredStopAtLevel=4" "-Xmx1g" "-server"]
        :plugins [[lein-cljfmt "0.7.0"]
                  [lein-pprint "1.3.2"]
                  [my-lein-templates "1.0.1"]
                  [my-lein-utils  "1.0.4"]]
        :repl-options {:init (do
                               (set! *print-level* 8)
                               (set! *print-length* 30))}
        :auto-refresh true}}
