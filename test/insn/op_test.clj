(ns insn.op-test
  (:require [insn.op :as op]
            [clojure.test :refer :all]))

(deftest test-compile
  (is (thrown-with-msg?
       Exception
       #"invalid arity for op :invokedynamic: expected 3 or 4 or 5, got 0"
       (op/compile [[:invokedynamic]]))))

(deftest test-op-seq
  (let [ops '[nil
              [:ldc 1]
              nil
              [nil
               (:ldc 2)
               [[nil]
                ([:iadd])
                [[:ldc 3]]
                ((:imul) [:ireturn])]
               (nil)]
              nil]]
    (is (= [[:ldc 1]
            [:ldc 2]
            [:iadd]
            [:ldc 3]
            [:imul]
            [:ireturn]]
           (op/op-seq ops)))))
