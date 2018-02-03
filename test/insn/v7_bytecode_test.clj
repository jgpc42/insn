(ns insn.v7-bytecode-test
  (:require [insn.core :as insn]
            [clojure.test :refer :all])
  (:import [java.lang.invoke
            CallSite ConstantCallSite MethodHandle
            MethodHandles MethodHandles$Lookup MethodType]))

(deftest test-invokedynamic
  (let [bdesc [MethodHandles$Lookup String MethodType CallSite]
        obj (insn/new-instance
             {:version 1.7
              :methods [{:flags [:public :static], :name "bsm", :desc bdesc
                         :emit [[:new ConstantCallSite]
                                [:dup]
                                [:invokestatic MethodHandles "lookup" [MethodHandles$Lookup]]
                                [:ldc :this]
                                [:aload 1]
                                [:ldc Integer]
                                [:ldc Integer]
                                [:invokestatic MethodType "methodType" [Class Class MethodType]]
                                [:invokevirtual MethodHandles$Lookup "findStatic" [Class String MethodType MethodHandle]]
                                [:aload 2]
                                [:invokevirtual MethodHandle "asType" [MethodType MethodHandle]]
                                [:invokespecial ConstantCallSite :init [MethodHandle :void]]
                                [:areturn]]}
                        {:flags [:public :static], :name "inc", :desc [Integer Integer]
                         :emit [[:aload 0]
                                [:invokevirtual Integer "intValue" [:int]]
                                [:ldc 1]
                                [:iadd]
                                [:invokestatic Integer "valueOf" [:int Integer]]
                                [:areturn]]}
                        {:name "go", :desc [:int :int]
                         :emit [[:iload 1]
                                [:invokedynamic "inc" [:int :int] [:invokestatic :this "bsm" bdesc]]
                                [:ireturn]]}]})]
    (is (= 43 (.go obj 42)))))
