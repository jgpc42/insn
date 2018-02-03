(ns insn.v9-bytecode-test
  (:require [insn.module :as module]
            [clojure.test :refer :all]))

(deftest test-modules
  (let [a {:id [:a "v1"]
           :flags [:open]
           :requires [:b]
           :uses ['pkg.c.isvc]}
        b {:id :b
           :exports [{:package :pkg.b
                      :modules [:a]}]
           :requires [{:id [:c "v2"]
                       :flags [:transitive]}]}
        c {:id [:c "v2"]
           :exports [:pkg.c]
           :opens [:pkg.c
                   {:package :pkg.c.priv
                    :modules [:b]}]
           :provides {'pkg.c.isvc ['pkg.c.svc]}}]
    ;; TODO: implement more rigorous test
    (is (every? :bytes (map module/generate [a b c])))))
