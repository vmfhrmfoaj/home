(defun cider-cljs-root-dirs ()
  (let (repl)
    (dolist (buf (cider-connections) repl)
      (with-current-buffer buf
        (when (string= "clj" cider-repl-type)
          (setq repl buf))))
    (when repl
      (ignore-errors
        (-some-> (concat "(let [b (some-> \"project.clj\""
                         "                (slurp)"
                         "                (clojure.edn/read-string)"
                         "                (some->> (drop 3) (apply hash-map))"
                         "                (get-in [:cljsbuild :builds]))"
                         "        b (cond-> b (map? b) (vals))]"
                         "  (some->> b"
                         "           (mapcat :source-paths)"
                         "           (apply hash-set)"
                         "           (seq)"
                         "           (map clojure.java.io/as-file)"
                         "           (map (memfn getAbsolutePath))))")
                 (nrepl-sync-request:eval repl)
                 (nrepl-dict-get "value")
                 (edn-read))))))
