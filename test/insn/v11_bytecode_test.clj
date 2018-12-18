(ns insn.v11-bytecode-test
  (:require [insn.core :as core]
            [insn.util :as util]
            [clojure.test :refer :all])
  (:import [java.lang.invoke MethodHandles$Lookup]))

(deftest test-constant-dynamic
  (let [bdesc [MethodHandles$Lookup String Class Object Object]
        this (str "pkg." (gensym "cd"))
        const #(util/constant-dynamic (str "foo-" %) :long
                                      [:invokestatic this "bsm" bdesc] [%])

        obj (core/new-instance
             {:version 11
              :name this
              :methods [{:flags [:public :static], :name "bsm", :desc bdesc
                         :emit [[:aload 3]
                                [:checkcast Long]
                                [:invokevirtual Long "longValue" [:long]]
                                [:ldc2 1]
                                [:ladd]
                                [:invokestatic Long "valueOf" [:long Long]]
                                [:areturn]]}
                        {:name "go", :desc [:long]
                         :emit [[:ldc (const 42)]
                                [:ldc (const 17)]
                                [:ladd]
                                [:lreturn]]}]})]
    (is (= (+ 42 1 17 1)
           (.go obj)))))
