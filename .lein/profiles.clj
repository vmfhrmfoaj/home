{:user {:dependencies [[com.cemerick/piggieback "0.2.1"]
                       [org.clojure/tools.nrepl "0.2.12"]]
        :plugins      [[lein-pprint "1.1.2"]
                       [cider/cider-nrepl "0.13.0-SNAPSHOT"]
                       [refactor-nrepl    "2.3.0-SNAPSHOT"]]
        :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}
