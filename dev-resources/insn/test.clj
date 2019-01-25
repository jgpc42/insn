(ns insn.test
  (:require [clojure.test :as test]
            [insn.core :as insn]
            insn.core-test
            insn.clojure-test
            insn.op-test
            insn.util-test
            insn.v7-bytecode-test)
  (:gen-class))

(def clj
  ((juxt :major :minor) *clojure-version*))

(def jvm
  (->> (.split (System/getProperty "java.specification.version")
               "\\.")
       last Long/valueOf))

(when (>= jvm 9)
  (require 'insn.v9-bytecode-test))

(when (>= jvm 11)
  (require 'insn.v11-bytecode-test))

;;;

(def passed (atom nil))

(defmethod test/report :begin-test-ns [m])

(defmethod test/report :summary [m]
  (reset! passed (:test m))
  (when-not (and (pos? (:test m))
                 (zero? (+ (:fail m) (:error m))))
    (throw (ex-info (str "tests failed for bytecode version "
                         insn/*bytecode-version*) m))))

(defn -main []
  (let [test-ns? #(re-find #"^insn\..+-test$" (str (ns-name %)))
        nses (filter test-ns? (all-ns))
        versions (vec (range 5 (inc jvm)))]
    (test/with-test-out
      (print "{")
      (pr :jvm jvm, :clj clj, :bytecode versions)
      (print " "))
    (doseq [ver versions]
      (binding [insn/*bytecode-version* ver]
        (apply test/run-tests nses)))
    (test/with-test-out
      (pr :passed @passed)
      (println "}"))))
