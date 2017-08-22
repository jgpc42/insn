(ns insn.test
  (:require [clojure.test :as test]
            [clojure.java.io :as io]
            [insn.core :as insn]))

(defn -main []
  (let [nses (->> (.getResources (.getClassLoader (class -main)) "insn")
                  enumeration-seq
                  (filter #(.endsWith (.getPath %) "/test/insn"))
                  (mapcat #(-> % .openStream io/reader line-seq))
                  (keep #(second (re-find #"^(.+)_test\.clj$" %)))
                  (map #(symbol (str "insn." % "-test"))))
        clj (apply str ((juxt :major (constantly \.) :minor) *clojure-version*))
        asm (if (= clojure.asm.Type (get (ns-imports 'insn.core) 'Type))
              "clojure.asm"
              "objectweb.asm")]
    (dorun (map require nses))
    (doseq [ver [1.5 1.6 1.7]]
      (binding [insn/*bytecode-version* ver]
        (println "\n\n-- Testing Clojure" clj "and" asm "with bytecode version" ver)
        (apply test/run-tests nses)))))
