(ns insn.util
  "Bytecode and ASM utilities."
  (:refer-clojure :exclude [sort type])
  (:import [org.objectweb.asm Handle Label Opcodes Type]
           [clojure.lang AFunction Keyword Sequential Symbol]))

(defonce ^:private dynalock (Object.))

(defn ^:internal ^:no-doc dynaload
  "Based on code from https://github.com/clojure/spec-alpha2. Used to
  require vars from feature namespaces that use classes that may not
  exist in older ASM versions."
  [feature s]
  (let [ns (namespace s)]
    (assert ns)
    (try
      (locking dynalock
        (require (symbol ns)))
      (catch Exception e
        (let [^String msg
              (or (some->
                   ^Throwable (some #(or (and (instance? ClassNotFoundException %) %)
                                         (and (nil? %) e))
                                    (iterate #(and % (.getCause ^Throwable %)) e))
                   .getMessage)
                  "")]
          (if (.startsWith msg "org.objectweb.asm.")
            (throw (ex-info (str "Unsupported ASM feature: " feature)
                            {:feature feature :var s} e))
            (throw e)))))
    (let [v (resolve s)]
      (assert v)
      v)))

(defn ^:internal ^:no-doc get-opcode
  "Try and get the integer opcode `fname` from the ASM `Opcodes' class.
  Return it if exists, else return `default`. Used to get opcodes that
  may not exist in older ASM versions."
  ([fname] (get-opcode fname nil))
  ([fname default]
   (try (.getInt (.getField Opcodes fname) nil)
        (catch NoSuchFieldException _ default))))

(defn ^:internal ^:no-doc import-class
  "Try and import the package-prefixed class name `cname`. Returns a
  class object or `default` if not found. Used to import ASM classes
  that may not exist in older ASM versions."
  ([cname] (import-class cname nil))
  ([cname default]
   (try (Class/forName cname)
        (catch ClassNotFoundException _ default))))

;;;

(defprotocol LabelArray
  (label-array [x]
    "Return a typed array of ASM Labels."))

(defprotocol ClassDesc
  (class-desc [x]
    "Return an internal package-prefixed class name string."))

(defprotocol TypeDesc
  (type-desc [x]
    "Return an internal type string."))

(defprotocol AsmType
  (type [x]
    "Return an ASM Type object."))

(defprotocol TypeSig
  (type-sig [x]
    "Return an internal type signature string."))

;;;

(def ^{:doc "The internal class name of the class being generated."
       :dynamic true}
  *this* nil)

(def ^{:doc "The internal superclass name of the class being generated."
       :dynamic true}
  *super* nil)

(def ^{:doc "The auto-generated labels of the method being emitted."
       :dynamic true}
  *labels* nil)

;;;

(def ^:no-doc array-type-keyword?
  {:boolean Opcodes/T_BOOLEAN
   :byte Opcodes/T_BYTE
   :char Opcodes/T_CHAR
   :short Opcodes/T_SHORT
   :int Opcodes/T_INT
   :long Opcodes/T_LONG
   :float Opcodes/T_FLOAT
   :double Opcodes/T_DOUBLE})

(def ^:no-doc class-keyword?
  {:this #'*this*
   :super #'*super*})

(def ^:no-doc flag-keyword?
  {:abstract Opcodes/ACC_ABSTRACT
   :annotation Opcodes/ACC_ANNOTATION
   :bridge Opcodes/ACC_BRIDGE
   :deprecated Opcodes/ACC_DEPRECATED
   :enum Opcodes/ACC_ENUM
   :final Opcodes/ACC_FINAL
   :interface Opcodes/ACC_INTERFACE
   :mandated Opcodes/ACC_MANDATED
   :module Opcodes/ACC_MODULE
   :native Opcodes/ACC_NATIVE
   :open Opcodes/ACC_OPEN
   :private Opcodes/ACC_PRIVATE
   :protected Opcodes/ACC_PROTECTED
   :public Opcodes/ACC_PUBLIC
   :static Opcodes/ACC_STATIC
   :static-phase Opcodes/ACC_STATIC_PHASE
   :strict Opcodes/ACC_STRICT
   :super Opcodes/ACC_SUPER
   :synchronized Opcodes/ACC_SYNCHRONIZED
   :synthetic Opcodes/ACC_SYNTHETIC
   :transient Opcodes/ACC_TRANSIENT
   :transitive Opcodes/ACC_TRANSITIVE
   :varargs Opcodes/ACC_VARARGS
   :volatile Opcodes/ACC_VOLATILE
   :record (get-opcode "ACC_RECORD")})

(def ^:no-doc handle-keyword?
  {:getfield Opcodes/H_GETFIELD
   :getstatic Opcodes/H_GETSTATIC
   :invokeinterface Opcodes/H_INVOKEINTERFACE
   :invokespecial Opcodes/H_INVOKESPECIAL
   :invokestatic Opcodes/H_INVOKESTATIC
   :invokevirtual Opcodes/H_INVOKEVIRTUAL
   :newinvokespecial Opcodes/H_NEWINVOKESPECIAL
   :putfield Opcodes/H_PUTFIELD
   :putstatic Opcodes/H_PUTSTATIC})

(def ^:no-doc method-keyword?
  {:init "<init>"
   :clinit "<clinit>"})

(def ^:no-doc type-keyword?
  {:boolean "Z"
   :byte "B"
   :char "C"
   :short "S"
   :int "I"
   :long "J"
   :float "F"
   :double "D"
   :void "V"})

(def ^:no-doc type-fn?
  {boolean "Z", booleans "[Z"
   byte "B", bytes "[B"
   char "C", chars "[C"
   short "S", shorts "[S"
   int "I", ints "[I"
   long "J", longs "[J"
   float "F", floats "[F"
   double "D", doubles "[D"})

(def ^:no-doc asm-type-keyword?
  {:boolean Type/BOOLEAN_TYPE
   :byte Type/BYTE_TYPE
   :char Type/CHAR_TYPE
   :double Type/DOUBLE_TYPE
   :float Type/FLOAT_TYPE
   :int Type/INT_TYPE
   :long Type/LONG_TYPE
   :short Type/SHORT_TYPE
   :void Type/VOID_TYPE})

(def ^:no-doc version?
  {1.1 Opcodes/V1_1, 1 Opcodes/V1_1
   1.2 Opcodes/V1_2, 2 Opcodes/V1_2
   1.3 Opcodes/V1_3, 3 Opcodes/V1_3
   1.4 Opcodes/V1_4, 4 Opcodes/V1_4
   1.5 Opcodes/V1_5, 5 Opcodes/V1_5
   1.6 Opcodes/V1_6, 6 Opcodes/V1_6
   1.7 Opcodes/V1_7, 7 Opcodes/V1_7
   1.8 Opcodes/V1_8, 8 Opcodes/V1_8
   9 Opcodes/V9
   10 (get-opcode "V10")
   11 (get-opcode "V11")
   12 (get-opcode "V12")
   13 (get-opcode "V13")
   14 (get-opcode "V14")
   15 (get-opcode "V15")
   16 (get-opcode "V16")})

(defmacro ^:no-doc check-valid
  "Get the value at `k` in map `m` or throw exception."
  [msg m k]
  `(let [m# ~m, k# ~k]
     (or (get m# k#)
         (throw (ex-info (str "invalid " ~msg ": " (pr-str k#))
                         {:key k# :map m#})))))

;;;

(defn array-type
  "Return an ASM array type operand opcode flag."
  [x]
  (check-valid "array type" array-type-keyword? x))

(defn flags
  "Return opcode bit flags for asm class, method, or field."
  [x]
  (let [flag #(check-valid "flag" flag-keyword? %)]
    (if (or (sequential? x) (set? x))
      (if (seq x)
        (apply bit-or 0 (map flag x))
        0)
      (flag x))))

(defn ^:no-doc intern-symbol?
  "Return true if the given value is a symbol with no namespace or dots."
  [x]
  (and (symbol? x)
       (nil? (namespace x))
       (not (.contains (name x) "."))))

(defn label
  "Return a new asm label. Note that labels are mutable values."
  [] (Label.))

(defn ^:internal ^:no-doc label-from
  "If `x` is a Label, return it. Otherwise, get or create a new label
  derived from `x`."
  [x]
  (if (instance? Label x)
    x
    (if (not *labels*)
      (throw (IllegalStateException.
              "auto-generated labels only available when emitting method bytecode"))
      (or (get @*labels* x)
          (let [lbl (label)]
            (swap! *labels* assoc x lbl)
            lbl)))))

(defn labels
  "Returns a sequence of `n` (or infinite) new asm labels."
  ([] (repeatedly label))
  ([n] (take n (labels))))

(defn method-desc
  "Return internal method descriptor string."
  [xs]
  (if (seq xs)
    (let [[args ret] [(butlast xs) (last xs)]
          args (map type-desc args)]
      (str "(" (apply str args) ")"
           (type-desc ret)))
    "()V"))

(defn method-name
  "Return a method name string."
  [x]
  (or (method-keyword? x)
      (name x)))

(defn method-type
  "Return an ASM Type object denoting a method."
  [xs]
  (Type/getMethodType (method-desc xs)))

(defn ^:no-doc optional
  "Accepts a predicate `f` and a seq `s`. If the first of s `x`
  satisfies f, return a tuple of x and the next of s. Otherwise,
  return a tuple of the given default value and s."
  ([pred xs] (optional pred xs nil))
  ([pred [x & more :as xs] default]
   (if (pred x)
     [x more]
     [default (seq xs)])))

(defn ^:no-doc resolve-if
  "Resolve the symbol if the resolved value satisfies the predicate."
  [pred s]
  (let [v (resolve s)]
    (when (pred v)
      v)))

(defn special-desc
  "Return internal type string for the 'anewarray', 'checkcast',
  'instanceof', or 'new' instructions."
  [x]
  (if (sequential? x)
    (type-desc x)
    (class-desc x)))

(defn handle
  "Return an ASM Handle object for a field or method."
  [tag owner mname desc-or-type]
  (let [field? (#{:getfield :getstatic :putfield :putstatic} tag)
        desc (if field? (type-desc desc-or-type) (method-desc desc-or-type))]
    (Handle. (check-valid "handle" handle-keyword? tag) (class-desc owner)
             (method-name mname) desc)))

(defn sort
  "Return the ASM sort number of the given type."
  [x]
  (.getSort ^Type (type x)))

(let [fref (delay (dynaload "constant-dynamic" 'insn.v11-feature/constant-dynamic))]
  (defn constant-dynamic
    "Return an ASM ConstantDynamic object. Note that using these values
    require version 11 bytecode and above."
    ([cname ftype boot]
     (constant-dynamic cname ftype boot []))
    ([cname ftype boot args]
     (let [boot (if (sequential? boot)
                  (apply handle boot)
                  boot)]
       (@fref (name cname) (type-desc ftype)
              boot (object-array args))))))

;;;

(extend-protocol LabelArray
  nil
  (label-array [_] (label-array []))
  Number
  (label-array [n] (make-array Label (long n)))
  Sequential
  (label-array [coll]
    (let [size (count coll)
          ^objects dest (make-array Label size)]
      (loop [i 0, s (seq coll)]
        (if (< i size)
          (do (aset dest i (label-from (first s)))
              (recur (inc i) (next s)))
          dest)))))

(extend (Class/forName (str "[L" (.getName Label) ";"))
  LabelArray {:label-array identity})

(extend-protocol ClassDesc
  Class
  (class-desc [c] (class-desc (.getName c)))
  Keyword
  (class-desc [k]
    (or @(check-valid "class keyword" class-keyword? k)
        (throw (IllegalStateException.
                (str k " class name proxy is only bound during bytecode emission")))))
  String
  (class-desc [s]
    (if (.contains s ".")
      (.replace s \. \/)
      s))
  Symbol
  (class-desc [s]
    (if-let [c (and (intern-symbol? s)
                    (resolve-if class? s))]
      (class-desc c)
      (class-desc (name s)))))

(extend-protocol TypeDesc
  AFunction
  (type-desc [f] (check-valid "type descriptor" type-fn? f))
  Class
  (type-desc [c]
    (cond
      (.isPrimitive c)
      (type-desc (keyword (.getName c)))
      (.isArray c)
      (type-desc [(.getComponentType c)])
      :else
      (type-desc (.getName c))))
  Keyword
  (type-desc [k]
    (or (some-> (class-keyword? k) deref type-desc)
        (check-valid "type descriptor" type-keyword? k)))
  Sequential
  (type-desc [s] (str "[" (type-desc (first s))))
  String
  (type-desc [s]
    (if (.endsWith s ";")
      s
      (str "L" (class-desc s) ";")))
  Symbol
  (type-desc [s]
    (if (= s 'void)
      "V"
      (if-let [x (and (intern-symbol? s)
                      (resolve s))]
        (type-desc (if (var? x) @x x))
        (type-desc (name s))))))

(extend-protocol AsmType
  Class
  (type [c] (Type/getType c))
  Keyword
  (type [k]
    (if (class-keyword? k)
      (type (class-desc k))
      (check-valid "asm type" asm-type-keyword? k)))
  Sequential
  (type [s] (type (type-desc s)))
  Symbol
  (type [s]
    (if-let [c (and (intern-symbol? s)
                    (resolve-if class? s))]
      (type c)
      (type (name s))))
  String
  (type [s] (Type/getType ^String (type-desc s)))
  Type
  (type [t] t))

(extend-protocol TypeSig
  nil
  (type-sig [_])
  String
  (type-sig [s] s))
