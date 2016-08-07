{:user {:dependencies [[com.cemerick/piggieback "0.2.1"]
                       [org.clojure/tools.nrepl "0.2.12"]
                       [figwheel-sidecar "0.5.3-2"]]
        :plugins      [[lein-pprint "1.1.2"]
                       [cider/cider-nrepl "0.14.0-SNAPSHOT"]
                       [refactor-nrepl "2.3.0-SNAPSHOT"]
                       [lein-cljfmt "0.5.3"]]
        :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}
