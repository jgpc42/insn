(ns insn.core-test
  (:require [insn.core :as core]
            [insn.op :as op]
            [clojure.test :refer :all])
  (:import [java.lang.annotation Retention RetentionPolicy Target ElementType]
           [java.lang.invoke CallSite ConstantCallSite MethodHandle MethodHandles MethodHandles$Lookup MethodType]
           [javax.annotation.processing SupportedOptions]
           [javax.xml.ws WebServiceRef WebServiceRefs]
           [javax.xml.ws.soap Addressing]))

(defn make [& {:as m}]
  (core/new-instance m))

(deftest test-casts
  (let [obj (make
             :methods
             [{:name "foo", :desc [Object String]
               :emit [[:aload 1]
                      [:checkcast String]
                      [:areturn]]}
              {:name "bar", :desc [Object [String]]
               :emit [[:aload 1]
                      [:checkcast [String]]
                      [:areturn]]}])]
    (testing "downcast"
      (is (= "ok" (.foo obj (#(do "ok"))))))

    (testing "array cast"
      (is (= "ok" (-> (.bar obj (#(into-array String ["ok"])))
                      (aget 0)))))))

(deftest test-fields
  (let [cname (symbol (str "insn.core_test." (gensym "fields")))
        obj (make
             :name cname
             :fields [{:flags [:public], :name "x", :type :int}
                      {:flags [:public :static], :name "y", :type Long}]
             :methods [{:name :clinit
                        :emit [[:ldc 17]
                               [:i2l]
                               [:invokestatic Long "valueOf" [:long Long]]
                               [:putstatic :this "y" Long]
                               [:return]]}
                       {:name :init
                        :emit [[:aload 0]
                               [:invokespecial :super :init [:void]]
                               [:aload 0]
                               [:ldc 42]
                               [:putfield :this "x" :int]
                               [:return]]}])]
    (testing "instance"
      (is (= 42 (.-x obj))))

    (testing "static"
      (is (= 17 (eval `(. ~cname y)))))))

(deftest test-ldc
  (let [obj (make
             :methods [{:name "go", :desc [:long :long :long]
                        :emit [[:lload 1]
                               [:lload 3]
                               [:ldc2 100]
                               [:ladd]
                               [:ladd]
                               [:lreturn]]}])]
    (is (= 159 (.go obj 42 17)))))

(deftest test-lookupswitch
  (let [obj (make
             :methods [{:name "go", :desc [:int Object]
                        :emit [[:iload 1]
                               [:lookupswitch :OR {42 :L1, 17 :L2}]
                               [:mark :L1] [:ldc "a"] [:areturn]
                               [:mark :L2] [:ldc "b"] [:areturn]
                               [:mark :OR] [:ldc nil] [:areturn]]}])]
    (testing "found case"
      (is (= "a" (.go obj 42)))
      (is (= "b" (.go obj 17))))

    (testing "default case"
      (is (= nil (.go obj 100))))))

(deftest test-methods
  (let [cname (str "insn.core_test." (gensym "methods"))
        obj (make
             :name cname
             :methods [{:name "foo", :desc [Long :int :int]
                        :emit [[:aload 1]
                               [:invokevirtual Long "intValue" [:int]]
                               [:iload 2]
                               [:iadd]
                               [:ireturn]]}
                       {:flags [:public :static], :name "bar", :desc [:int :int]
                        :emit [[:iinc 0 1]
                               [:iload 0]
                               [:ireturn]]}])]
    (testing "instance"
      (is (= 59 (.foo obj 42 17))))

    (testing "static"
      (is (= 43 (eval `(~(symbol cname "bar") 42)))))))

(deftest test-multianewarray
  (let [obj (make
             :methods [{:name "go", :desc [Object]
                        :emit [[:ldc 1]
                               [:ldc 2]
                               [:multianewarray [[String]] 2]
                               [:astore 1]
                               [:aload 1]
                               [:ldc 0]
                               [:aaload]
                               [:ldc 1]
                               [:ldc "foo"]
                               [:aastore]
                               [:aload 1]
                               [:areturn]]}])]
    (is (= "foo" (-> (.go obj) (aget 0) (aget 1))))))

(deftest test-newarray
  (let [obj (make
             :methods [{:name "go", :desc [:int]
                        :emit [[:ldc 2]
                               [:newarray :int]
                               [:ldc 1]
                               [:iaload]
                               [:ireturn]]}])]
    (is (= 0 (.go obj)))))

(deftest test-tableswitch
  (let [emit (fn [v]
               (let [[OR L1 L2 :as labels] (range 3)
                     ret #(doto % (op/ldc %2) op/iadd op/ireturn)]
                 (doto v
                   (op/iload 1) (op/dup)
                   (op/tableswitch -1 0 OR (next labels))
                   (op/mark L1) (ret 1)
                   (op/mark L2) (ret 10)
                   (op/mark OR) (op/ineg) (op/ireturn))))
        obj (make
             :methods [{:name "go", :desc [:int :int]
                        :emit emit}])]
    (testing "found case"
      (is (= 0 (.go obj -1)))
      (is (= 10 (.go obj 0))))

    (testing "default case"
      (is (= -1 (.go obj 1))))))

(deftest test-trycatch
  (let [obj (make
             :methods [{:name "go", :desc [String :int :int]
                        :emit [[:mark :BEGIN]
                               [:aload 1]
                               [:iload 2]
                               [:invokevirtual String "charAt" [:int :char]]
                               [:ireturn]
                               [:mark :END]
                               [:mark :NULL]
                               [:pop] [:ldc -1] [:ireturn]
                               [:mark :IOOB]
                               [:pop1] [:ldc -2] [:ireturn]
                               [:trycatch :BEGIN :END :NULL NullPointerException]
                               [:trycatch :BEGIN :END :IOOB IndexOutOfBoundsException]]}])]
    (testing "no exception"
      (is (= 97 (.go obj "a" 0))))

    (testing "handle exception"
      (is (= -1 (.go obj nil 0)))
      (is (= -2 (.go obj "a" 1))))))

(deftest test-virtual
  (let [itype (core/define
                {:flags #{:public :interface}
                 :methods [{:flags #{:public :abstract}
                            :name :my_method
                            :desc [:int]}]})
        obj (make
             :flags #{:public}
             :interfaces [itype]
             :methods [{:flags #{:public}
                        :name :toString
                        :desc [String]
                        :emit [[:ldc "ok"]
                               [:areturn]]}
                       {:flags #{:public}
                        :name :my_method
                        :desc [:int]
                        :emit [[:ldc 42]
                               [:ireturn]]}])]
    (testing "impl"
      (is (instance? itype obj))
      (is (= 42 (.my-method obj))))

    (testing "override"
      (is (= "ok" (str obj))))))

(deftest test-annotations
  (let [anns {Deprecated true
              Retention RetentionPolicy/RUNTIME
              SupportedOptions ["foo" "bar" "baz"]
              Addressing {:enabled false, :required true}
              WebServiceRefs [`(WebServiceRef {:name "fred", :type String})
                              `(WebServiceRef {:name "ethel", :mappedName "lucy"})]}
        klass (core/define
                {:flags [:public :final]
                 :annotations anns
                 :fields [{:flags [:public :final], :name "b", :type :int, :annotations anns}]
                 :methods [{:flags [:public], :name "foo", :desc [String :int]
                            :annotations anns
                            :parameter-annotations {0 {Deprecated true}}
                            :emit [[:aload 0]
                                   [:getfield :this "b" :int]
                                   [:ireturn]]}]})
        foo (.getMethod klass "foo" (into-array Class [String]))]

    (testing "type and member"
      (doseq [t [klass, (.getField klass "b"), foo]]
        (is (= 5 (count (.getAnnotations t))))
        (is (.getAnnotation t Deprecated))
        (is (= RetentionPolicy/RUNTIME
               (.value (.getAnnotation t Retention))))
        (is (= ["foo" "bar" "baz"]
               (seq (.value (.getAnnotation t SupportedOptions)))))
        (is (= [false true]
               ((juxt #(.enabled %) #(.required %))
                (.getAnnotation t Addressing))))
        (is (= [["fred" java.lang.String ""]
                ["ethel" java.lang.Object "lucy"]]
               (->> (.getAnnotation t WebServiceRefs) .value
                    (map (juxt #(.name %) #(.type %) #(.mappedName %))))))))

    (testing "parameter"
      (is (= [Deprecated]
             (map #(.annotationType %)
                  (aget (.getParameterAnnotations foo) 0)))))))

(deftest test-dynamic
  (let [bdesc [MethodHandles$Lookup String MethodType CallSite]
        obj (make
             :version 1.7
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
                               [:ireturn]]}])]
    (is (= 43 (.go obj 42)))))

(deftest test-debug-info
  (let [obj (-> {:methods [{:name "inc", :desc [:int :int]
                            :emit [[:mark :BEGIN]
                                   [:line-number 1 :BEGIN]
                                   [:iload 1]
                                   [:ldc 1]
                                   [:iadd]
                                   [:ireturn]
                                   [:mark :END]
                                   [:local-variable "this" :this :BEGIN :END 0]
                                   [:local-variable "i" :int :BEGIN :END 1]]}]}
                core/visit
                #_(core/write (System/getProperty "java.io.tmpdir"))
                core/new-instance)]
    (is (= 43 (.inc obj 42)))))

(deftest test-new-instance
  (let [put (fn [s]
              [[:aload 0]
               [:dup]
               [:invokespecial :super :init [:void]]
               [:ldc s]
               [:putfield :this "s" String]
               [:return]])
        type (core/visit
              {:fields [{:name "s", :type String}]
               :methods [{:name :init, :desc [:void], :emit (put "a")}
                         {:name :init, :desc [Object :void], :emit (put "b")}
                         {:name :init, :desc [String :long :void], :emit (put "c")}
                         {:name :init, :desc [String Long :void], :emit (put "d")}
                         {:name :init, :desc [Object Object :void], :emit (put "e")}
                         {:name :toString, :desc [String]
                          :emit [[:aload 0]
                                 [:getfield :this "s" String]
                                 [:areturn]]}]})]
    (is (= "a" (str (core/new-instance type))))
    (is (= "b" (str (core/new-instance type "foo"))))
    (is (= "d" (str (core/new-instance type "bar" 42))))
    (is (= "e" (str (core/new-instance type "baz" (int 42)))))))
