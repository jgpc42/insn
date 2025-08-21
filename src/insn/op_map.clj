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

(def ^:private op-handlers
  "Each added tuple should be an op keyword seq, an emit fn, and a fn
  that converts the map op to the vector equivalent. Nothing fancy like
  multimethods/protocols for performance here."
  (let [add #(vary-meta (merge %1 (zipmap %2 (repeat %3)))
                        merge (zipmap %2 (repeat %4)))]
    (-> {}
        (add [:aload :astore
              :dload :dstore
              :fload :fstore
              :iload :istore
              :lload :lstore
              :ret]
             (fn [^MethodVisitor v op]
               (.visitVarInsn v (op-opcode op) (util/local-index (:index op))))
             (juxt :op :index))

        (add [:anewarray :checkcast :instanceof :new]
             (fn [^MethodVisitor v op]
               (.visitTypeInsn v
                               (op-opcode op)
                               (util/special-desc (:type op))))
             (juxt :op :type))

        (add [:getfield :getstatic :putfield :putstatic]
             (fn [^MethodVisitor v op]
               (.visitFieldInsn v
                                (op-opcode op)
                                (util/class-desc (:class op))
                                (name (:name op))
                                (util/type-desc (:type op))))
             (juxt :op :class :name :type))

        (add [:goto :jsr
              :ifnull :ifnonnull
              :ifeq :ifne
              :ifge :ifgt
              :ifle :iflt
              :if-acmpeq :if-acmpne
              :if-icmpeq :if-icmpne
              :if-icmpge :if-icmpgt
              :if-icmple :if-icmplt]
             (fn [^MethodVisitor v op]
               (.visitJumpInsn v (op-opcode op) (util/label-from (:label op))))
             (juxt :op :label))

        (add [:iinc]
             (fn [^MethodVisitor v op]
               (.visitIincInsn v (util/local-index (:index op)) (:value op)))
             (juxt :op :index :value))

        (add [:invokedynamic]
             (fn [^MethodVisitor v {b :bootstrap :as op}]
               (.visitInvokeDynamicInsn
                v
                (util/method-name (:name op))
                (util/method-desc* (param-types op) (return-type op))
                (if (util/handle? b)
                  b
                  (util/method-handle (:op b) (:class b) (:name b)
                                      (param-types b) (return-type b)))
                (object-array (:args op))))
             (fn [{args :args b :bootstrap :as op}]
               (cond-> [(:op op) (:name op)
                        (util/make-desc (param-types op) (return-type op))
                        (if (util/handle? b)
                          b
                          [(:op b) (:class b) (:name b)
                           (util/make-desc (param-types op) (return-type op))])]
                 args
                 (conj args))))

        (add [:invokeinterface :invokespecial :invokestatic :invokevirtual]
             (fn [^MethodVisitor v op]
               (let [code (op-opcode op)
                     ret (if (== Opcodes/INVOKESPECIAL code)
                           :void
                           (return-type op))
                     iface? (boolean (:interface op (== Opcodes/INVOKEINTERFACE code)))]
                 (.visitMethodInsn v code
                                   (util/class-desc (:class op))
                                   (util/method-name (:name op))
                                   (util/method-desc* (param-types op) ret)
                                   iface?)))
             (fn [{iface :interface :as op}]
               (cond-> [(:op op) (:class op) (:name op)
                        (util/make-desc (param-types op) (return-type op))]
                 iface
                 (conj iface))))

        (add [:ldc]
             (fn [v op] (op/ldc v (:value op)))
             (juxt :op :value))

        (add [:ldc2]
             (fn [v op] (op/ldc2 v (:value op)))
             (juxt :op :value))

        (add [:line-number]
             (fn [^MethodVisitor v op]
               (.visitLineNumber v (:value op) (util/label-from (:label op))))
             (juxt :op :value :label))

        (add [:local-variable]
             (fn [^MethodVisitor v op]
               (.visitLocalVariable v
                                    (name (:name op))
                                    (util/type-desc (:type op))
                                    nil
                                    (util/label-from (:start-label op))
                                    (util/label-from (:end-label op))
                                    (util/local-index (:index op))))
             (juxt :op :name :type :start-label :end-label :index))

        (add [:lookupswitch]
             (fn [^MethodVisitor v op]
               (let [[keys labels] (lookupswitch-pair op)]
                 (.visitLookupSwitchInsn v
                                         (util/label-from (:default-label op))
                                         (int-array keys)
                                         (util/label-array labels))))
             (fn [op]
               (let [[keys labels] (lookupswitch-pair op)]
                 [(:op op) (:default-label op) keys labels])))

        (add [:mark]
             (fn [^MethodVisitor v op]
               (.visitLabel v (util/label-from (:label op))))
             (juxt :op :label))

        (add [:multianewarray]
             (fn [^MethodVisitor v op]
               (.visitMultiANewArrayInsn v
                                         (util/type-desc (:type op))
                                         (:dimensions op)))
             (juxt :op :type :dimensions))

        (add [:newarray]
             (fn [^MethodVisitor v op]
               (.visitIntInsn v
                              Opcodes/NEWARRAY
                              (util/array-type (:type op))))
             (juxt :op :type))

        (add [:tableswitch]
             (fn [^MethodVisitor v op]
               (.visitTableSwitchInsn v (:min op) (:max op)
                                      (util/label-from (:default-label op))
                                      (util/label-array (:labels op))))
             (juxt :op :min :max :default-label :labels))

        (add [:trycatch]
             (fn [^MethodVisitor v op]
               (.visitTryCatchBlock v
                                    (util/label-from (:start-label op))
                                    (util/label-from (:end-label op))
                                    (util/label-from (:handler-label op))
                                    (when-let [t (:type op)]
                                      (util/class-desc t))))
             (juxt :op :start-label :end-label :handler-label :type))

        (add [:nop
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
             (fn [^MethodVisitor v op]
               (.visitInsn v (op-opcode op)))
             (juxt :op)))))

(def op?
  "Returns true if the given value is a valid op map."
  (comp keyword? :op))

(def op-seq
  "Return a flattened sequence of map ops."
  (partial op/op-seq op?))

(defn emit-op
  "Emit the map-style `op` to the given ASM MethodVisitor.

  The supported op maps are as follows. For more documentation on each
  :op key, please see the corresponding fn in the `insn.op` ns.

    The loads/stores, along with :ret; e.g.,

     {:op :aload :index 0}

    The :new, :instanceof, :checkcast and array allocator ops; e.g.,

     {:op :checkcast :type String}
     {:op :newarray :type :int}
     {:op :anewarray :type Long}
     {:op :multianewarray :type Object :dimensions 3}

    Field getters/setters. The :class key here in the owner class; e.g.,

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

    Additionally, the :line-number and :local-variable ops; e.g.,

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
  [v op]
  (let [k (:op op)
        f (op-handlers k)]
    (if (nil? f)
      (throw (ex-info (str "invalid map :op key: " k) op))
      (f v op))))

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

(defn op-vec
  "Convert a map-style op to the more compact vector representation."
  [op]
  (let [k (:op op)
        f ((meta op-handlers) k)]
    (if (nil? f)
      (throw (ex-info (str "invalid map :op key: " k) op))
      (f op))))
