{:user {:plugins [[lein-create-template "0.2.0"]
                  [lein-cljfmt "0.5.3"]
                  [lein-count  "1.0.7"]
                  [lein-pprint "1.1.2"]
                  [lein-project-version "0.1.0"]
                  [lein-dev-helper  "0.1.1"]
                  [lein-test-helper "0.1.2"]]
        :repl-options {:init (do
                               (set! *print-level* 8)
                               (set! *print-length* 30))}}}
