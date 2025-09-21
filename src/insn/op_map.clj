(ns insn.op-map
  "Eager map-based method op bytecode emission.

  Experimental and subject to change as of version 0.6.x.

  This ns supplies fns for emitting ops of the form {:op op-name ...},
  instead of [op-name ...]."
  (:require [clojure.walk :as walk]
            [insn.op :as op]
            [insn.util :as util]))

(defn- param-types [op]
  (or (:params op) (:parameter-types op)))

(defn- return-type [op]
  (or (:type op) (:return-type op)))

(defmulti emit-op
  "Emit the map-style `op` to the given ASM MethodVisitor.

  The default supported op maps are as follows. For more documentation
  on each :op key, please see the corresponding fn in the `insn.op` ns.

    The loads/stores, along with :ret; e.g.,

     {:op :aload :index 0}

    The :new, :instanceof, :checkcast and array allocator ops; e.g.,

     {:op :checkcast :type String}
     {:op :newarray :type :int}
     {:op :anewarray :type Long}
     {:op :multianewarray :type Object :dimensions 3}

    Field getters/setters. The :class key here is the owner class; e.g.,

      {:op :getfield :class :this :name 'field :type String}

    (Un)conditional jumps, along with the associated :mark; e.g.,

      {:op :goto :label :L}
      {:op :if-null :label :NULL}
      {:op :mark :label :HERE}

    The special integer increment; e.g.,

      {:op :iinc :index 2 :value 1}

    The invoke ops. Like the field ops, :class is the owner class. The
    :interface flag is also available when the owner class is of an
    interface type (when calling default methods); e.g.,

     {:op :invokespecial :class Object :name :init}
     {:op :invokestatic
      :class Long
      :name 'valueOf
      :parameter-types [Long] :return-type :long}

    The :invokedynamic op is like a normal invoke op with a :bootstrap
    method instead of an owner :class. The :args key is optional; e.g.,

      {:op :invokedynamic
       :name 'method
       :params [String] :type [:int]
       :bootstrap {:op :invokestatic :class :this ...}
       :args [42]}

    The single and double width constant load ops; e.g.;

      {:op :ldc :value \"str\"}
      {:op :ldc :value 42}       ;; an int
      {:op :ldc :value 17.1}     ;; a float

      {:op :ldc2 :value 42}      ;; a long
      {:op :ldc2 :value 17.1}    ;; a double

    The :line-number and :local-variable ops; e.g.,

      {:op :line-number :value 42 :label :LINE}
      {:op :local-variable
       :name 'var
       :type String
       :start-label :START
       :end-label :END
       :index 1}

    The table lookup ops. For :lookupswitch, the :keys and :labels can
    be specified via an :entries map instead of as a pair; e.g.,

      {:op :tableswitch :min 1 :max 10 :default-label :L :labels [...]}
      {:op :lookupswitch :default-label :L :entries {...}}

    For :trycatch, if the :type key is omitted, all Throwable types will
    be handled; e.g.,

      {:op :trycatch
       :start-label :START
       :end-label :END
       :handler-label :HANDLE
       :type IllegalArgumentException}

    Finally, the plethora single-byte ops. These have only a name; e.g.,

      {:op :iadd}
      {:op :areturn}
      ..."
  (fn [v op] (:op op))
  :default ::invalid)

(defmacro ^:private defops
  ([ops mkeys]
   `(do ~@(for [op ops]
            (let [f (symbol "insn.op" (name op))]
              `(defmethod emit-op ~op [v# {:keys ~mkeys}]
                 (~f v# ~@mkeys))))))
  ([ops argv & body]
   `(do ~@(for [op ops]
            (let [f (symbol "insn.op" (name op))]
              `(defmethod emit-op ~op ~argv
                 ~@(walk/postwalk-replace {'&fn f} body)))))))

(defops [:aload :astore
         :dload :dstore
         :fload :fstore
         :iload :istore
         :lload :lstore
         :ret]
  [index])

(defops [:anewarray :checkcast :instanceof :new] [type])

(defops [:getfield :getstatic :putfield :putstatic] [class name type])

(defops [:goto :jsr
         :ifnull :ifnonnull
         :ifeq :ifne
         :ifge :ifgt
         :ifle :iflt
         :if-acmpeq :if-acmpne
         :if-icmpeq :if-icmpne
         :if-icmpge :if-icmpgt
         :if-icmple :if-icmplt]
  [label])

(defops [:iinc] [index value])

(defops [:invokedynamic]
  [v {b :bootstrap :as op}]
  (let [params (param-types op)
        ret-type (return-type op)
        boot-op (:op b)
        boot-ret (if (= boot-op :invokespecial) :void (return-type b))
        handle (util/method-handle boot-op (:class b) (:name b)
                                   (param-types b) boot-ret)]
    (op/invokedynamic v (:name op) params ret-type handle (:args op))))

(defops [:invokeinterface :invokespecial :invokestatic :invokevirtual]
  [v {k :op :as op}]
  (let [ret-type (if (= k :invokespecial) :void (return-type op))
        iface (or (:interface op) (= k :invokeinterface))]
    (&fn v (:class op) (:name op) (param-types op) ret-type iface)))

(defops [:ldc] [value])

(defops [:ldc2] [value])

(defops [:line-number] [value label])

(defops [:local-variable] [name type start-label end-label index])

(defops [:lookupswitch]
  [v op]
  (let [[keys labels] (if-let [ks (:keys op)]
                        [ks (:labels op)]
                        (let [m (:entries op)
                              ks (keys m)]
                          (if (sorted? m)
                            [ks (vals m)]
                            (let [ks (sort ks)]
                              [ks (map m ks)]))))]
    (op/lookupswitch* v (:default-label op) keys labels)))

(defops [:mark] [label])

(defops [:multianewarray] [type dimensions])

(defops [:newarray] [type])

(defops [:tableswitch] [min max default-label labels])

(defops [:trycatch] [start-label end-label handler-label type])

(defops [:nop
         :iaload :laload :faload :daload
         :aaload :baload :caload :saload
         :iastore :lastore :fastore :dastore
         :aastore :bastore :castore :sastore
         :pop :pop2
         :dup :dup-x1 :dup-x2
         :dup2 :dup2-x1 :dup2-x2
         :swap
         :iadd :ladd :fadd :dadd
         :isub :lsub :fsub :dsub
         :imul :lmul :fmul :dmul
         :idiv :ldiv :fdiv :ddiv
         :irem :lrem :frem :drem
         :ineg :lneg :fneg :dneg
         :ishl :lshl
         :ishr :lshr :iushr :lushr
         :iand :land
         :ior :lor
         :ixor :lxor
         :i2l :i2f :i2d
         :l2i :l2f :l2d
         :f2i :f2l :f2d
         :d2i :d2l :d2f
         :i2b :i2c :i2s
         :lcmp :fcmpl :fcmpg :dcmpl :dcmpg
         :ireturn :lreturn :freturn :dreturn :areturn :return
         :arraylength
         :athrow
         :monitorenter :monitorexit]
  [])

(def op?
  "Returns true if the given value is a valid op map."
  (comp keyword? :op))

(def op-seq
  "Return a flattened sequence of map ops."
  (partial op/op-seq op?))

(defn emit-seq
  "Emit the map-style op sequence to the given ASM MethodVisitor.

  Nested sequences are traversed in depth-first order, and nils ignored.

  For performance, this fn emits nested sequences eagerly via recursion.
  Therefore, lazily flatten very deeply nested sequences beforehand with
  `op-seq`, or use `emit-op` manually."
  [v ops]
  (loop [[x & xs] ops]
    (cond
      (map? x) (do (emit-op v x)
                   (recur xs))
      (seq x) (do (emit-seq v x)
                  (recur xs))
      (seq xs) (recur xs))))
