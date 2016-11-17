{:user {:dependencies [[org.clojure/tools.nrepl "0.2.12"]]
        :plugins [[cider/cider-nrepl "0.15.0-SNAPSHOT"]
                  [com.jakemccrary/lein-test-refresh "0.17.0"]
                  [lein-cider-figwheel-helper "0.0.1"]
                  [lein-cljfmt "0.5.3"]
                  [lein-expectations-plus "0.0.2"]
                  [lein-pprint "1.1.2"]
                  [refactor-nrepl "2.3.0-SNAPSHOT"]]
        :repl-options {:init (do
                               (set! *print-level* 8)
                               (set! *print-length* 30))}}}
