{:user {:plugins [[lein-create-template "0.2.0"]
                  [lein-cljfmt "0.5.3"]
                  [lein-count  "1.0.7"]
                  [lein-pprint "1.1.2"]
                  [lein-dev-helper  "0.0.3-SNAPSHOT"]
                  [lein-test-helper "0.0.4-SNAPSHOT"]]
        :repl-options {:init (do
                               (set! *print-level* 8)
                               (set! *print-length* 30))}}}
