{:user {:plugins [[lein-cljfmt "0.7.0"]
                  [lein-pprint "1.3.2"]
                  [my-lein-templates "1.0.1"]
                  [my-lein-utils  "1.0.3"]]
        :middleware [dev-helper.plugin/middleware]
        :repl-options {:init (do
                               (set! *print-level* 8)
                               (set! *print-length* 30))}
        :auto-refresh true}}
