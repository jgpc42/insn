(ns insn.op-map
  "Eager map-based method op bytecode emission.

  Experimental and subject to change as of version 0.6.x.

  This ns supplies fns for emitting ops of the form {:op op-name ...},
  instead of [op-name ...]."
  (:require [insn.op :as op]
            [insn.util :as util])
  (:import [org.objectweb.asm Opcodes MethodVisitor]))

(defn- op-opcode ^long [op]
  (op/keyword-opcode (:op op)))

(defn- param-types [op]
  (or (:params op) (:parameter-types op)))

(defn- return-type [op]
  (or (:type op) (:return-type op)))

(defn- lookupswitch-pair [op]
  (if-let [ks (:keys op)]
    [ks (:labels op)]
    (let [m (:entries op)
          ks (keys m)]
      (if (sorted? m)
        [ks (vals m)]
        (let [ks (sort ks)]
          [ks (map m ks)])))))

(defmulti emit-op
  "Emit the map-style `op` to the given ASM MethodVisitor.

  The supported op maps are as follows. For more documentation on each
  :op dispatch key, please see the corresponding fn in the `insn.op` ns.

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

    Finally, the plethora single-byte ops. These have only an :op; e.g.,

      {:op :iadd}
      {:op :areturn}
      ..."
  (fn [v op] (:op op))
  :default ::default)

(defmulti op-vec
  "Convert a map-style op to the more compact vector representation."
  :op, :default ::default)

(defmacro ^:private def-op-methods [ops argv emit-body vec-body]
  `(do ~@(for [op ops]
           `(do (defmethod emit-op ~op
                  ~(update argv 0 vary-meta assoc :tag `MethodVisitor)
                  ~emit-body)
                (defmethod op-vec ~op [~(second argv)]
                  ~vec-body)))))

(def-op-methods [:aload :astore
                 :dload :dstore
                 :fload :fstore
                 :iload :istore
                 :lload :lstore
                 :ret]
  [v op]
  , (.visitVarInsn v (op-opcode op) (util/local-index (:index op)))
  , (mapv op [:op :index]))

(def-op-methods [:anewarray :checkcast :instanceof :new]
  [v op]
  , (.visitTypeInsn v (op-opcode op) (util/special-desc (:type op)))
  , (mapv op :op :type))

(def-op-methods [:getfield :getstatic :putfield :putstatic]
  [v op]
  , (let [cdesc (util/class-desc (:class op))
          tdesc (util/type-desc (:type op))]
      (.visitFieldInsn v (op-opcode op) cdesc (name (:name op)) tdesc))
  , (mapv op [:op :class :name :type]))

(def-op-methods [:goto :jsr
                 :ifnull :ifnonnull
                 :ifeq :ifne
                 :ifge :ifgt
                 :ifle :iflt
                 :if-acmpeq :if-acmpne
                 :if-icmpeq :if-icmpne
                 :if-icmpge :if-icmpgt
                 :if-icmple :if-icmplt]
  [v op]
  , (.visitJumpInsn v (op-opcode op) (util/label-from (:label op)))
  , (mapv op [:op :label]))

(def-op-methods [:iinc]
  [v op]
  , (.visitIincInsn v (util/local-index (:index op)) (:value op))
  , (mapv op [:op :index :value]))

(def-op-methods [:invokedynamic]
  [v {args :args b :bootstrap :as op}]
  , (let [mname (util/method-name (:name op))
          desc (util/method-desc* (param-types op) (return-type op))
          handle (if (util/handle? b)
                   b
                   (util/method-handle (:op b) (:class b) (:name b)
                                       (param-types b) (return-type b)))]
      (.visitInvokeDynamicInsn v mname desc handle (object-array (:args op))))
  , (cond-> [(:op op) (:name op)
             (util/make-desc (param-types op) (return-type op))
             (if (util/handle? b)
               b
               [(:op b) (:class b) (:name b)
                (util/make-desc (param-types op) (return-type op))])]
      args
      (conj args)))

(def-op-methods [:invokeinterface :invokespecial :invokestatic :invokevirtual]
  [v {iface :interface :as op}]
  , (let [code (op-opcode op)
          cdesc (util/class-desc (:class op))
          mname (util/method-name (:name op))
          ret (if (== Opcodes/INVOKESPECIAL code)
                :void
                (return-type op))
          mdesc (util/method-desc* (param-types op) ret)
          iface? (boolean (or iface (== Opcodes/INVOKEINTERFACE code)))]
      (.visitMethodInsn v code cdesc mname mdesc iface?))
  , (cond-> [(:op op) (:class op) (:name op)
             (util/make-desc (param-types op) (return-type op))]
      iface
      (conj iface)))

(def-op-methods [:ldc]
  [v op]
  , (op/ldc v (:value op))
  , (mapv op [:op :value]))

(def-op-methods [:ldc2]
  [v op]
  , (op/ldc2 v (:value op))
  , (mapv op [:op :value]))

(def-op-methods [:line-number]
  [v op]
  , (.visitLineNumber v (:value op) (util/label-from (:label op)))
  , (mapv op [:op :value :label]))

(def-op-methods [:local-variable]
  [v op]
  , (let [tdesc (util/type-desc (:type op))
          slabel (util/label-from (:start-label op))
          elabel (util/label-from (:end-label op))
          idx (util/local-index (:index op))]
      (.visitLocalVariable v (name (:name op)) tdesc nil slabel elabel idx))
  , (mapv op [:op :name :type :start-label :end-label :index]))

(def-op-methods [:lookupswitch]
  [v op]
  , (let [[keys labels] (lookupswitch-pair op)]
      (.visitLookupSwitchInsn v
                              (util/label-from (:default-label op))
                              (int-array keys)
                              (util/label-array labels)))
  , (let [[keys labels] (lookupswitch-pair op)]
      [(:op op) (:default-label op) keys labels]))

(def-op-methods [:mark]
  [v op]
  , (.visitLabel v (util/label-from (:label op)))
  , (mapv op [:op :label]))

(def-op-methods [:multianewarray]
  [v op]
  , (.visitMultiANewArrayInsn v (util/type-desc (:type op)) (:dimensions op))
  , (mapv op [:op :type :dimensions]))

(def-op-methods [:newarray]
  [v op]
  , (.visitIntInsn v Opcodes/NEWARRAY (util/array-type (:type op)))
  , (mapv op [:op :type]))

(def-op-methods [:tableswitch]
  [v op]
  , (let [dlabel (util/label-from (:default-label op))
          labels (util/label-array (:labels op))]
      (.visitTableSwitchInsn v (:min op) (:max op) dlabel labels))
  , (mapv op [:op :min :max :default-label :labels]))

(def-op-methods [:trycatch]
  [v op]
  , (let [slabel (util/label-from (:start-label op))
          elabel (util/label-from (:end-label op))
          hlabel (util/label-from (:handler-label op))
          cdesc (when-let [t (:type op)]
                             (util/class-desc t))]
      (.visitTryCatchBlock v slabel elabel hlabel cdesc))
  , (mapv op [:op :start-label :end-label :handler-label :type]))

(def-op-methods [:nop
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
  [v op]
  , (.visitInsn v (op-opcode op))
  [(:op op)])

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
