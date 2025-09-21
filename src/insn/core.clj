(ns insn.core
  "Simple JVM bytecode generation."
  (:require [insn.util :as util]
            [insn.annotation :as ann]
            [insn.op :as op]
            [clojure.java.io :as io])
  (:import [clojure.lang DynamicClassLoader RT]
           [org.objectweb.asm ClassVisitor ClassWriter]
           [java.io File FileOutputStream]
           [java.lang.reflect Constructor]))

(def ^{:private true, :tag 'double}
  jvm-version
  (-> (System/getProperty "java.specification.version")
      Double/valueOf))

(def ^{:doc "The class/interface flags to use if unspecified."
       :dynamic true}
  *class-flags* #{:public :final})

(def ^{:doc "The field flags to use if unspecified."
       :dynamic true}
  *field-flags* #{:private :final})

(def ^{:doc "The constructor flags to use if unspecified."
       :dynamic true}
  *init-flags* #{:public})

(def ^{:doc "The method flags to use if unspecified."
       :dynamic true}
  *method-flags* #{:public :final})

(def ^{:doc "The bytecode version to use for types if unspecified."
       :dynamic true}
  *bytecode-version*
  (if (>= jvm-version 1.7) 7 6))

;;;

(declare visit-field visit-method)

(defn visit
  "Generate the class bytecode from the provided type map. Returns a map
  of the class's :name and :bytes. Options:

    :name         of the class. Optional, but see below.

    :flags        seq of class/interface modifier flags (e.g., :final).
                  See the `insn.util` namespace.

    :super        defaults to Object.

    :interfaces   sequence of interface types to extend/implement.

    :annotations  map or sequence of tuples, described below.

    :signature    generic type information - usually only ever needed
                  for interop with Java libraries (reflection, etc.)

    :fields       sequence of field maps, described below.

    :methods      sequence of method maps, described below.

    :version      bytecode version given as a integer. For backwards
                  compatibility, a float of major.minor may be given
                  for versions 1.1 through 1.8.

    :source       string denoting the original source file - optional.

    :debug        string of arbitrary debug information - optional.

    :emit-fn      fn to be called to emit method bytecode. This is an
                  advanced option and, if present, will be passed both
                  the ASM MethodVisitor and the raw :emit value for each
                  method. Can be used to support new op data types. For
                  example, the `insn.op-map/emit-seq` fn can be used to
                  enable map-style op sequences.

  Each field and method can also be given :annotations and a :signature
  as per above.

  Some examples:

    (visit {:flags #{:public :interface}
            :name 'my.ns.Foo
            :methods [{:flags #{:public :abstract}
                       :name :my_method
                       :desc [:int]}]})
    (visit {:flags #{:public}
            :name 'my.ns.Bar
            :interfaces ['my.ns.Foo]
            :methods [{:flags #{:public}
                       :name :toString
                       :desc [String]
                       :emit [[:ldc \"Bar\"]
                              [:areturn]]}
                      {:flags #{:public}
                       :name :my_method
                       :desc [:int]
                       :emit [[:ldc 42]
                              [:ireturn]]}]})

  Class instance/static fields are provided as maps. Options:

    :name   field name (required).

    :type   field type (required).

    :flags  seq of field modifier flags.

    :value  initial value. Only for primitive and String static fields,
            and if given, must be a corresponding Integer, Float, Long,
            Double, or String value.

  Some example maps:

    {:flags #{:public :final}, :name :my_string, :type String}
    {:flags #{:static}, :name :some_number, :type :long, :value 42}

  Class/interface methods, constructors, and static initializers are
  provided as maps. Options:

    :name   method name (required). Can be either :init or :clinit,
            designating a constructor or the static initializer,
            respectively.

    :flags  seq of method modifier flags. Ignored for the static
            initializer.

    :desc   method parameter types and return type (specified last).
            Ignored for the static initializer, optional for
            constructors. For constructors, the method return type is
            forced to void if not explicitly specified as such.

            Alternatively, the separate keys :params and :type can be
            used for the parameter types and return type, respectively.
            For normal methods, :type is required. For :init, the
            (possibly empty) :params key is required and :type is
            ignored. If either is used, they take priority over :desc.
            (Aliases: :parameter-types, :return-type.)

    :emit   either a fn taking a MethodVisitor or a sequence of
            instructions to emit for the method (see `insn.op`).
            Optional if method is abstract. (Alias: :bytecode.)

  Some example maps, :emit has been omitted for brevity:

    {:name :add_ints, :desc [:int :int :int]}
    {:flags #{:private}, :name :init, :desc [String :boolean :void]}
    {:flags #{:private}, :name :init, :params [String :boolean] :type :void}
    {:name :clinit}

  Additionally, methods may be given :parameter-annotations provided as
  a map of {parameter-index annotations}. (Alias: :param-annotations.)

  If a class name is not given, a generated (gensym) class name is used,
  qualified by the current namespace.

  If the type does not define at least one constructor, and is not an
  interface type, a default, zero-argument constructor with default
  access will be written that simply invokes the superclass constructor.
  Note that if the super class does not support this constructor an
  error will be raised.

  All annotations are provided as a map or sequence of tuples. Each key
  is the Annotation name and each value is a map of elements. A non-map
  value specifies a single element named :value as per java.

  Annotation values are processed the same as in clojure.
  See: https://clojure.org/reference/datatypes#_java_annotation_support"
  [t]
  (let [cls (some-> (:name t) name)
        cls (or cls (str (namespace-munge *ns*) "."
                         (gensym "insn_type")))
        flags (set (seq (:flags t *class-flags*)))
        flags (if (:interface flags) (conj flags :abstract) flags)
        this (util/class-desc cls)
        super (util/class-desc (:super t Object))
        ifaces (map util/class-desc (:interfaces t))

        version (:version t *bytecode-version*)
        bversion (util/check-valid "version" util/version? version)
        iversion (if (float? version)
                   (* 10 (- (double version) (long version)))
                   version)
        wflags (if (>= (long iversion) 7)
                 ClassWriter/COMPUTE_FRAMES
                 ClassWriter/COMPUTE_MAXS)

        cv (doto (ClassWriter. wflags)
             (.visit bversion (util/flags flags) this (util/type-sig (:signature t))
                     super (into-array String ifaces))
             (.visitSource (:source t) (:debug t)))

        ctor? #(= "<init>" (util/method-name (:name %)))
        t (if (or (:interface flags) (some ctor? (:methods t)))
            t
            (update t :methods conj
                    {:name :init
                     :emit [[:aload 0]
                            [:invokespecial :super :init [:void]]
                            [:return]]}))
        emit-fn (:emit-fn t op/emit-seq)]
    (binding [util/*this* this
              util/*super* super]
      (doseq [f (:fields t)]
        (visit-field cv f))
      (doseq [m (:methods t)]
        (visit-method cv m emit-fn))
      (doto cv
        (ann/visit (:annotations t))
        .visitEnd))
    {:bytes (.toByteArray cv), :name cls, :insn/visited true}))

(defn- visit-field [^ClassVisitor cv f]
  (let [flags (:flags f *field-flags*)
        ftype (util/type-desc (:type f))
        fval (when-let [v (:value f)]
               (case ftype
                 "I" (int v), "J" (long v)
                 "F" (float v), "D" (double v)
                 v))
        fv (.visitField cv (util/flags flags)
                        (name (:name f)) ftype (util/type-sig (:signature f)) fval)]
    (ann/visit fv (:annotations f))
    (.visitEnd fv)))

(defn- visit-method [^ClassVisitor cv m emit-fn]
  (let [mname (util/method-name (:name m))
        clinit? (= mname "<clinit>")
        init? (= mname "<init>")
        flags (cond
                clinit? [:static]
                init? (:flags m *init-flags*)
                :else (:flags m *method-flags*))
        desc (cond
               clinit?
               "()V"

               init?
               (if (contains? m :params)
                 (util/method-desc* (:params m) :void)
                 (util/constructor-desc (:desc m)))

               :else
               (if-let [ret (or (:type m) (:return-type m))]
                 (util/method-desc* (or (:params m) (:parameter-types m)) ret)
                 (if-let [desc (:desc m)]
                   (util/method-desc desc)
                   "()V")))
        mv (doto (.visitMethod cv (util/flags flags) mname desc
                               (util/type-sig (:signature m)) nil)
             (.visitCode))
        emit (or (:emit m) (:bytecode m))]
    (binding [util/*labels* (atom {})]
      (if (fn? emit)
        (emit mv)
        (emit-fn mv emit)))
    (ann/visit mv (:annotations m))
    (doseq [[i anns] (or (:parameter-annotations m) (:param-annotations m))]
      (ann/visit mv i anns))
    (doto mv
      (.visitMaxs -1 -1)
      .visitEnd)))

(def ^{:arglists '([t])
       :doc "Alias of `visit`."}
  generate visit)

;;;

(defprotocol Loader
  (load-type [cl t]
    "Return a class object from the given map containing the class
    :bytes and :name."))

(extend-protocol Loader
  DynamicClassLoader
  (load-type [cl t]
    (.defineClass cl (:name t) (:bytes t) nil)))

(defn- ensure-visited [t]
  (if (:insn/visited t) t (visit t)))

(defn get-bytes
  "Return a representation of the provided type as an array of bytes.
  This array is an in-memory equivalent to a java .class file on disk."
  ^bytes [t]
  (:bytes (ensure-visited t)))

(defn define
  "Return a Class object from the provided type."
  (^Class [t]
   (define (RT/makeClassLoader) t))
  (^Class [cl t]
   (load-type cl (ensure-visited t))))

(defn new-instance
  "Define and return an instance of the generated class. The
  non-abstract type must define a public constructor that accepts
  either no arguments, or if `args` is provided, the given arguments.

  If the given argument count matches a constructor with a distinct
  arity, that one is invoked. In this case, constructors that take
  primitives are supported via reflective unboxing.

  Otherwise, the argument types must exactly match (as per
  `clojure.core/class`) a defined constructor, or a constructor that
  accepts the same amount of `Object`s."
  ([t]
   (-> t define .newInstance))
  ([t & args]
   (let [klass (define t)
         make (partial into-array Class)
         ctors (for [^Constructor c (.getConstructors klass)
                     :when (== (count args) (alength (.getParameterTypes c)))]
                 c)
         ctor (if (== (count ctors) 1)
                (first ctors)
                (try
                  (.getConstructor klass (make (map class args)))
                  (catch NoSuchMethodException e
                    (let [objs (make (repeat (count args) Object))]
                      (try
                        (.getConstructor klass objs)
                        (catch NoSuchMethodException _
                          (throw (ex-info "no constructor found for arguments"
                                          {:args args} e))))))))]
     (.newInstance ^Constructor ctor (object-array args)))))

(defn write
  "Takes a map specifying a class's :name and :bytes. Writes the class
  bytes to a .class file. The file's directory is taken from its
  package, its filename from its name, rooted at `root`.

  If not given, `root` defaults to `*compile-path*`, or if not set, the
  \"java.io.tmpdir\" property. If both of those are not set, a 'classes'
  directory in the current directory is used."
  ([t]
   (write t (or *compile-path*
                (System/getProperty "java.io.tmpdir", "classes"))))
  ([t root]
   (let [paths (.split ^String (:name t) "\\.")
         ^File dir (apply io/file root (butlast paths))
         file (io/file dir (str (last paths) ".class"))]
     (.mkdirs dir)
     (doto (FileOutputStream. file)
       (.write ^bytes (:bytes t))
       .close)
     t)))
