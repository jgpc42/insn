(ns insn.util-test
  (:require [insn.util :as util]
            [clojure.test :refer :all]))

(deftest test-class-desc
  (testing "special keywords"
    (binding [util/*this* "Foo"
              util/*super* "Bar"]
      (is (= "Foo" (util/class-desc :this)))
      (is (= "Bar" (util/class-desc :super)))))

  (testing "class objects"
    (is (= "java/lang/String"
           (util/class-desc String))))

  (testing "strings"
    (is (= "some/pkg/Quux"
           (util/class-desc "some.pkg.Quux")))))

(deftest test-flags
  (is (= (util/flags [:final :public])
         (+ (util/flags [:final])
            (util/flags [:public])))))

(deftest test-method-desc
  (is (= "(JLjava/lang/String;)B"
         (util/method-desc [:long String :byte]))))

(deftest test-method-name
  (testing "special keywords"
    (is (= "<init>" (util/method-name :init)))
    (is (= "<clinit>" (util/method-name :clinit))))

  (testing "strings"
    (is (= "foo" (util/method-name "foo")))))

(deftest test-special-desc
  (testing "class objects"
    (is (= "java/lang/Long"
           (util/special-desc Long))))

  (testing "type seqs"
    (is (= "[F" (util/special-desc [:float])))
    (is (= "[Ljava/lang/Float;"
           (util/special-desc (list Float))))))

(deftest test-type-desc
  (testing "special keywords"
    (is (= "Z" (util/type-desc :boolean)))
    (is (= "V" (util/type-desc :void))))

  (testing "special fns/symbols"
    (is (= "F" (util/type-desc 'float)))
    (is (= "V" (util/type-desc 'void)))
    (is (= "[J" (util/type-desc longs))))

  (testing "strings"
    (is (= "Lsome/pkg/Quux;"
           (util/type-desc "some.pkg.Quux"))))

  (testing "class objects"
    (is (= "Ljava/lang/String;"
           (util/type-desc String))))

  (testing "type seqs"
    (is (= "[I" (util/type-desc (list :int))))
    (is (= "[[Ljava/lang/Long;"
           (util/type-desc [[Long]]))))

  (testing "special keywords"
    (binding [util/*this* "Foo"]
      (is (= "LFoo;" (util/type-desc :this))))))
