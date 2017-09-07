(ns insn.test
  (:require [clojure.test :as test]
            [insn.core :as insn]
            insn.core-test
            insn.clojure-test
            insn.op-test
            insn.util-test)
  (:gen-class))

(defmethod test/report :begin-test-ns [m])

(defmethod test/report :summary [m]
  (when-not (and (pos? (:test m))
                 (zero? (+ (:fail m) (:error m))))
    (throw (ex-info (str "tests failed for bytecode version "
                         insn/*bytecode-version*) m))))

(defn -main []
  (let [clj (Double/valueOf
             (apply str ((juxt :major (constantly \.) :minor) *clojure-version*)))
        asm (when (not= clojure.asm.Type (get (ns-imports 'insn.core) 'Type))
              :external)
        aot (try (Class/forName "insn.test")
                 :static
                 (catch ClassNotFoundException _))
        nses (filter #(re-find #"^insn\..+-test$" (str (ns-name %))) (all-ns))
        versions [1.5 1.6 1.7]]
    (test/with-test-out
      (pr (vec (keep identity [clj aot asm])))
      (print " ") (flush))
    (doseq [ver versions]
      (binding [insn/*bytecode-version* ver]
        (apply test/run-tests nses)))
    (prn :ok)))
