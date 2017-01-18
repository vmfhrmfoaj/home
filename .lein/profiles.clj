{:user {:plugins [[lein-cljfmt "0.5.3"]
                  [lein-dev-helper "0.0.2-SNAPSHOT"]
                  [lein-expectations-plus "0.0.3-SNAPSHOT"]
                  [lein-pprint "1.1.2"]]
        :repl-options {:init (do
                               (set! *print-level* 8)
                               (set! *print-length* 30))}}}
