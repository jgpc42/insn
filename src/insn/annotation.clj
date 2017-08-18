(ns ^:no-doc insn.annotation
  "Annotation visitor fns."
  (:require [insn.util :as util])
  (:load "imports"))

(declare visit*)

(defn- visit1
  "Mostly taken from `clojure.core/add-annotation`."
  [^AnnotationVisitor v ename x]
  (cond
    (vector? x)
    (let [av (.visitArray v ename)]
      (doseq [xval x]
        (visit1 av "value" xval))
      (.visitEnd av))

    (seq? x)
    (let [[nname nval] x
          ndesc (util/type-desc (resolve nname))
          av (.visitAnnotation v ename ndesc)]
      (visit* av nval))

    :else
    (let [x (if (symbol? x) (eval x) x)]
      (cond
        (instance? Enum x)
        (let [edesc (util/type-desc (.getDeclaringClass ^Enum x))]
          (.visitEnum v ename edesc (str x)))

        (class? x)
        (.visit v ename (util/type x))

        :else
        (.visit v ename x)))))

(defn- visit*
  "Apply each annotation map entry to `visit1` and finalize the
  annotation visitor."
  [^AnnotationVisitor av aval]
  (let [aval (if (map? aval) aval {"value" aval})]
    (doseq [[k v] aval]
      (visit1 av (name k) v))
    (.visitEnd av)))

(defn ^:internal visit
  "Based on `clojure.core/add-annotations`."
  ([xv anns] (visit xv -1 anns))
  ([xv ^long idx anns]
   (doseq [[aname aval] (seq anns)]
     (let [aclass (if (class? aname)
                    aname
                    (resolve (symbol aname)))
           visible? (when-let [^Retention r (.getAnnotation ^Class aclass Retention)]
                      (= (.value r) RetentionPolicy/RUNTIME))
           adesc (util/type-desc aclass)
           av (condp instance? xv
                MethodVisitor
                (if (>= idx 0)
                  (.visitParameterAnnotation ^MethodVisitor xv idx adesc visible?)
                  (.visitAnnotation ^MethodVisitor xv adesc visible?))
                FieldVisitor
                (.visitAnnotation ^FieldVisitor xv adesc visible?)
                ClassVisitor
                (.visitAnnotation ^ClassVisitor xv adesc visible?))]
       (visit* av aval)))))
