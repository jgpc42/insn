(ns insn.test
  (:require [clojure.test :as test]
            [clojure.java.io :as io]
            [insn.core :as insn]
            insn.clojure)
  (:gen-class))

(defmethod test/report :begin-test-ns [m])

(defmethod test/report :summary [m]
  (when-not (and (pos? (:test m))
                 (zero? (+ (:fail m) (:error m))))
    (throw (ex-info "tests failed" m))))

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
              "objectweb.asm")
        aot (try (Class/forName "insn.test") "aot compiled"
                 (catch ClassNotFoundException _ "interpreted"))
        versions [1.5 1.6 1.7]]
    (dorun (map require nses))
    (test/with-test-out
      (print "Testing bytecode versions"
             (str (first versions) \- (last versions))
             "using Clojure" clj "and" asm "when" aot "... ")
      (flush))
    (doseq [ver versions]
      (binding [insn/*bytecode-version* ver]
        (apply test/run-tests nses)))
    (println "ok")))
