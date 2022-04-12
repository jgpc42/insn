(ns insn.v8-bytecode-test
  (:require [insn.core :as insn]
            [clojure.test :refer :all]))

(let [jvm-8 (->> (.split (System/getProperty "java.specification.version") "\\.")
                 last Long/valueOf (= 8))
      t {:version 8
         :interfaces '[java.util.Iterator]
         :methods [{:name "hasNext" :desc [:boolean]
                    :emit [[:ldc 1] [:ireturn]]}
                   {:name "remove" :desc [:void]
                    :emit [[:aload 0]
                           [:invokespecial java.util.Iterator :remove [:void]]
                           [:return]]}]}
      obj1 (insn/new-instance t)
      obj2 (insn/new-instance (update-in t [:methods 1 :emit 1] conj true))]
  (if jvm-8
    (deftest test-methods-interface-default
      (is (true? (.hasNext obj1)))
      (is (thrown? UnsupportedOperationException (.remove obj1)))
      (is (thrown? UnsupportedOperationException (.remove obj2))))
    (deftest test-methods-interface-default
      (is (true? (.hasNext obj1)))
      (is (thrown? IncompatibleClassChangeError (.remove obj1)))
      (is (thrown? UnsupportedOperationException (.remove obj2))))))
