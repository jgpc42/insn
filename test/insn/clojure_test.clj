(ns insn.clojure-test
  (:require [clojure.test :refer :all]
            [demo.core :as demo]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(deftest test-demo
  (testing "unboxed"
    (is (== 44 (inc (demo/incr 42))))
    (is (== 22.0 (demo/calc 22 nil)))
    (is (== 42.0 (demo/calc 22 :ok)))

    (testing "multi-arity"
      (is (== 43 (inc (demo/add 42))))
      (is (== 60 (inc (demo/add 42 17))))))

  (testing "boxed"
    (is (= [43] (map demo/incr [42])))

    (testing "multi-arity"
      (is (= [42] (map demo/add [42]))))))

(set! *unchecked-math* false)
(set! *warn-on-reflection* false)
