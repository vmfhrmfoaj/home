{:user {:dependencies [[org.clojure/tools.nrepl "0.2.12"]
                       [com.cemerick/piggieback "0.2.1"]
                       [figwheel-sidecar "0.5.8"]]
        :plugins [[cider/cider-nrepl "0.15.0-SNAPSHOT"]
                  [lein-autoexpect "1.9.0"]
                  [lein-cljfmt "0.5.3"]
                  [lein-pprint "1.1.2"]
                  [refactor-nrepl "2.3.0-SNAPSHOT"]]
        :repl-options {:init (do
                               (set! *print-level* 8)
                               (set! *print-length* 30))
                       :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}
