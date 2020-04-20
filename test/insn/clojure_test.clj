(ns insn.clojure-test
  (:require [clojure.test :refer :all]
            [insn.clojure :as bc]
            [demo.core :as demo]))

(deftest test-simple
  (is (= 43
         ((bc/fn [x]
            [[:aload 1]
             [:invokestatic clojure.lang.Numbers "inc" [Object Number]]
             [:areturn]])
          42))))

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
      (is (= [42] (map demo/add [42])))))

  (testing "variadic"
    (is (= 3 (demo/add 0 1 2)))
    (is (= 45 (apply demo/add (range 10))))))

(set! *unchecked-math* false)
(set! *warn-on-reflection* false)
