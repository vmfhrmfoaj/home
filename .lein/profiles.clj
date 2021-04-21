{:dev {:test-refresh {:changes-only true
                      :timeout-in-sec 5
                      :notify-command ["notify-send" "--hint" "int:transient:1" "Test"]}}
 :repl {:auto-refresh {:notify-command ["notify-send" "--hint" "int:transient:1" "Reload"]}
        :jvm-opts ["-XX:MaxGCPauseMillis=5" "-Xmx2g" "-server"]
        :middleware [dev-helper.plugin/middleware]}
 :user {:plugins [[lein-cljfmt "0.7.0"]
                  [lein-pprint "1.3.2"]
                  [my-lein-templates "1.0.1"]
                  [my-lein-utils  "1.0.4"]]
        :repl-options {:init (do
                               (set! *print-level* 8)
                               (set! *print-length* 30))}}}
