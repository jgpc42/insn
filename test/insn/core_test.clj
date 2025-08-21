(ns insn.core-test
  (:require [insn.core :as core]
            [insn.op :as op]
            [insn.op-map :as op-map]
            [clojure.test :refer :all])
  (:import [org.objectweb.asm ClassReader ClassVisitor MethodVisitor Opcodes]
           [java.lang.annotation Retention RetentionPolicy Target ElementType]
           [javax.annotation.processing SupportedOptions]
           [javax.xml.ws WebServiceRef WebServiceRefs]
           [javax.xml.ws.soap Addressing]
           [java.lang.reflect ParameterizedType]))

(defn make [& {:as m}]
  (core/new-instance m))

(deftest test-anewarray
  (let [obj (make
             :methods [{:name "go", :desc [[Object]]
                        :emit [[:ldc 1]
                               [:anewarray Object]
                               [:dup]
                               [:ldc 0]
                               [:ldc "foo"]
                               [:aastore]
                               [:areturn]]}])]
    (is (= "foo" (-> (.go obj) (aget 0))))))

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
  (let [cname 'insn.core_test.fields
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
                               [:invokespecial :super :init []]
                               [:aload 0]
                               [:ldc 42]
                               [:putfield :this "x" :int]
                               [:return]]}])]
    (testing "instance"
      (is (= 42 (.-x obj))))

    (testing "static"
      (is (= 17 (eval `(. ~cname y)))))))

(deftest test-instanceof
  (let [obj (make
             :methods [{:name "go", :desc [Object Object :int]
                        :emit [[:aload 1]
                               [:instanceof String]
                               [:aload 2]
                               [:instanceof [Long]]
                               [:iadd]
                               [:ireturn]]}])]
    (is (= 2 (.go obj "foo" (into-array Long [42]))))))

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
  (let [cname 'insn.core_test.methods
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
      (is (= 43 (eval `(. ~cname bar 42)))))))

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

(deftest test-trycatch-any
  (let [obj (make
             :fields [{:flags [:public], :name "i", :type :int}]
             :methods [{:name "go", :desc [String :int :int Throwable]
                        :emit [[:ldc nil]
                               [:astore 4]
                               [:mark :BEGIN]
                               [:aload 1]
                               [:iload 2]
                               [:invokevirtual String "charAt"]
                               [:pop]
                               [:mark :END]
                               [:goto :POST]
                               [:mark :ERROR]
                               [:astore 4]
                               [:mark :POST]
                               [:aload 0]
                               [:iload 3]
                               [:putfield :this "i" :int]
                               [:aload 4]
                               [:areturn]
                               [:trycatch :BEGIN :END :ERROR nil]]}])]
    (is (nil? (.go obj "a" 0 1)))
    (is (= 1 (.i obj)))
    (is (instance? IndexOutOfBoundsException (.go obj "a" 1 2)))
    (is (= 2 (.i obj)))
    (is (instance? NullPointerException (.go obj nil 0 1)))
    (is (= 1 (.i obj)))))

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

(deftest test-debug-info
  (let [t (core/visit
           {:methods [{:name "inc", :desc [:int :int]
                       :emit [[:mark :BEGIN]
                              [:line-number 1 :BEGIN]
                              [:iload 1]
                              [:ldc 1]
                              [:iadd]
                              [:ireturn]
                              [:mark :END]
                              [:local-variable "this" :this :BEGIN :END 0]
                              [:local-variable "i" :int :BEGIN :END 1]]}]})
        obj (core/new-instance t)

        debug (volatile! {})
        cv (proxy [ClassVisitor] [Opcodes/ASM4]
             (visitMethod [api mname & args]
               (when (= mname "inc")
                 (proxy [MethodVisitor] [Opcodes/ASM4]
                   (visitLineNumber [line & args]
                     (vswap! debug update :lines (fnil conj []) line))
                   (visitLocalVariable [vname & args]
                     (vswap! debug update :vars (fnil conj []) vname))))))
        _ (.accept (ClassReader. (:bytes t)) cv 0)]

    (is (= 43 (.inc obj 42)))
    (is (= [1] (:lines @debug)))
    (is (= ["this" "i"] (:vars @debug)))))

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
                         {:name :init, :desc [String :int :void], :emit (put "e")}
                         {:name :init, :desc [Object Object :void], :emit (put "f")}
                         {:name :init, :desc [:long :long :long :void], :emit (put "g")}
                         {:name :toString, :desc [String]
                          :emit [[:aload 0]
                                 [:getfield :this "s" String]
                                 [:areturn]]}]})]
    (is (= "a" (str (core/new-instance type))))
    (is (= "b" (str (core/new-instance type "foo"))))
    (is (= "d" (str (core/new-instance type "bar" 42))))
    (is (= "f" (str (core/new-instance type "baz" (int 42)))))
    (is (= "g" (str (core/new-instance type 1 (int 2) 3))))))

(deftest test-type-reflection
  (testing "field"
    (let [obj (core/new-instance
               {:methods [{:name "go", :desc [Object]
                           :emit [[:getstatic System "out"]
                                  [:areturn]]}]})]
      (is (= System/out (.go obj)))))

  (testing "method"
    (let [obj (core/new-instance
               {:methods [{:name "bar", :desc [[:char] Object]
                           :emit [[:ldc "foo-"]
                                  [:aload 1]
                                  [:ldc 1]
                                  [:ldc 2]
                                  [:invokestatic String "valueOf" 3]
                                  [:invokevirtual String "concat"]
                                  [:areturn]]}]})]
      (is (= "foo-bc" (.bar obj (.toCharArray "abcd")))))))

(deftest test-signature
  (let [
        ;; List<String> foo
        field-sig "Ljava/util/List<Ljava/lang/String;>;"

        ;; <T extends Iterable> List<T> bar (List<T> arg)
        method-sig "<T::Ljava/lang/Iterable;>(Ljava/util/List<TT;>;)Ljava/util/List<TT;>;"

        List java.util.List
        klass (core/define
                {:flags [:public]
                 :fields [{:flags [:public], :name "foo", :type List, :signature field-sig}]
                 :methods [{:flags [:public], :name "bar", :desc [List List] :signature method-sig
                            :emit [[:aload 0] [:areturn]]}]})]
    (is (-> klass (.getField "foo")
            ^ParameterizedType (.getGenericType) .getActualTypeArguments first (= String)))
    (is (= "T" (-> klass (.getMethod "bar" (into-array Class [List]))
                   ^ParameterizedType (.getGenericReturnType) .getActualTypeArguments first str)))))

(deftest test-interface-no-ctor
  (is (= 0 (-> (core/define {:flags #{:interface}})
               (.getDeclaredConstructors)
               (count)))))

(deftest test-keyword-aliases
  (let [c (core/define
            {:name 'T :flags #{:public :abstract}
             :methods [{:name 'm1 :params [String] :type :void
                        :param-annotations {0 {Deprecated true}}
                        :bytecode [[:return]]}
                       {:name 'm2 :parameter-types [String] :return-type :void
                        :bytecode [[:return]]}]})]
    (is (= "T" (.getName c)))
    (is (= 1 (count (.getDeclaredConstructors c))))
    (is (= [{:name "m1" :params [String] :type Void/TYPE :anns [Deprecated]}
            {:name "m2" :params [String] :type Void/TYPE :anns []}]
           (for [m (sort-by #(.getName %) (.getDeclaredMethods c))]
             {:name (.getName m)
              :params (vec (.getParameterTypes m))
              :type (.getReturnType m)
              :anns (map #(.annotationType %) (aget (.getParameterAnnotations m) 0))})))))

(deftest test-seq-emit-fn
  (let [t {:seq-emit-fn op-map/emit-seq
           :methods [{:name :init :params []
                      :emit [{:op :aload :index 0}
                             {:op :invokespecial :class :super :name :init}
                             {:op :return}]}
                     {:name :clinit
                      :emit [{:op :return}]}
                     {:name 'm :type Object
                      :emit [{:op :ldc, :value 0}
                             {:op :istore, :index 1}
                             ,
                             {:op :iinc, :index 1, :value 1}
                             ,
                             {:op :iload :index 1}
                             {:op :newarray, :type :int}
                             {:op :ifnull :label :L}
                             ,
                             {:op :goto, :label :L}
                             {:op :mark, :label :L}
                             ,
                             {:op :ldc2, :value 42}
                             {:op :invokestatic
                              :class Long
                              :name 'valueOf
                              :params [:long]
                              :type Long}
                             ,
                             {:op :areturn}]}]}]
    (is (= 42 (.m (core/new-instance t))))))
