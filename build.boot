(set-env!
 :resource-paths  #{"src/clj" "test"}
 :target-path     "target"
 :dependencies '[[org.clojure/clojure "1.9.0-alpha8"]
                 [org.clojure/core.async  "0.2.385"]
                 [org.clojure/test.check  "0.9.0"    :scope "test"]
                 [deraen/boot-ctn         "0.1.0"    :scope "test"]])

(require '[deraen.boot-ctn :refer  [init-ctn!]])

(init-ctn!)
