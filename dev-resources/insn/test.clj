(ns insn.test
  (:require [clojure.test :as test]
            [insn.core :as insn]
            insn.core-test
            insn.clojure-test
            insn.op-test
            insn.util-test)
  (:gen-class))

(def clj
  (-> (apply str ((juxt :major (constantly \.) :minor) *clojure-version*))
      Double/valueOf))

(def jvm
  (-> (System/getProperty "java.specification.version")
      Double/valueOf))

(when (>= jvm 1.7)
  (require 'insn.v7-bytecode-test))

(when (>= jvm 9)
  (require 'insn.v9-bytecode-test))

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
        versions (cond-> [5 6]
                   (>= jvm 1.7) (conj 7)
                   (>= jvm 1.8) (conj 8)
                   (>= jvm 9) (conj 9))]
    (test/with-test-out
      (print "{")
      (pr :jvm jvm, :clj clj, :bytecode versions)
      (print " "))
    (doseq [ver versions]
      (binding [insn/*bytecode-version* ver]
        (apply test/run-tests nses)))
    (pr :passed @passed)
    (println "}")))
