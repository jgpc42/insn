(ns insn.op
  "Bytecode helpers. Fns for opcodes with an underscore in their name
  (e.g., IF_ICMPEQ) use a dash instead (e.g., if-icmpeq)."
  (:refer-clojure :exclude [compile pop])
  (:require [insn.util :as util])
  (:import [org.objectweb.asm ConstantDynamic Handle Opcodes MethodVisitor]
           [java.lang.reflect Field Method Modifier]))

(defmulti ^:private -op
  "Return an op data map representing a op visitor fn to be called
  later. Takes a seq where the first item is a op keyword and the
  remaining items are the op arguments."
  first)

(defmacro ^:private def-op-method
  "Helper to define arity-checked `-op` methods."
  [vname]
  (let [kop (keyword vname)]
    `(defmethod -op ~kop [opseq#]
       (let [var# #'~vname
             args# (if (vector? opseq#)
                     (subvec opseq# 1)
                     (vec (next opseq#)))
             nargs# (count args#)
             arity# (->> var# meta :arglists
                         (map (comp dec count))
                         set)]
         (if (arity# nargs#)
           {::fn @var#, ::name ~kop, ::args args#}
           (let [arities# (if (== 1 (count arity#))
                            (first arity#)
                            (apply str (interpose " or " (sort arity#))))]
             (throw (ex-info (format "invalid arity for op %s: expected %s, got %d"
                                     ~kop arities# nargs#)
                             {:name ~kop, :args args#}))))))))

(defmacro ^:private defops
  "Define bytecode visitor fns taking an ASM MethodVisitor object as
  their first argument. A local `&op` value will be in scope that refers
  to the corresponding Opcode field. Additionally, a local `&fn` value
  is also in scope, denoting the fn."
  [ops doc & decls]
  (let [decls (if (seq? (first decls))
                decls
                (list decls))
        alists (map first decls)]
    `(do
       ~@(for [sym ops]
           (let [field (->> sym name .toUpperCase
                            (str (.getName Opcodes) \/)
                            symbol)
                 fname (-> sym name (.replace \_ \-) symbol)]
             `(do
                (def ~fname ~doc
                  (fn ~'&fn
                    ~@(for [[[v & args] & body] decls]
                        `([~(vary-meta v assoc :tag `MethodVisitor)
                           ~@args]
                          (let [~'&op ~field]
                            ~@body)))))
                (alter-meta! #'~fname assoc :arglists '~alists)
                (def-op-method ~fname)))))))

;;;

(defops [getfield putfield
         getstatic putstatic]
  "Read or write type `ftype` instance or static field named `fname` of
  class `cls`.

  If the type is not provided, it will be determined through reflection.
  In this case, the class must be given as a string or Class object."
  ([v cls fname]
   (let [fname (name fname)
         cls (if (class? cls)
               cls
               (Class/forName cls))
         static? (not (#{Opcodes/GETFIELD Opcodes/PUTFIELD} &op))
         types (for [^Field f (.getFields ^Class cls)
                     :when (and (= fname (.getName f))
                                (= static?
                                   (Modifier/isStatic (.getModifiers f))))]
                 (.getType f))]
     (if (== 1 (count types))
       (&fn v cls fname (first types))
       (let [msg (format "no %s field found with name '%s'"
                         (if static? "static" "instance") fname)]
         (throw (ex-info msg {:candidates types}))))))
  ([v cls fname ftype]
   (.visitFieldInsn v &op (util/class-desc cls)
                    (name fname) (util/type-desc ftype))))

(defops [iinc]
  "Increment int local variable index `idx` by amount `n`."
  [v idx n]
  (.visitIincInsn v idx n))

(defops [bipush sipush]
  "Push int value `n`."
  [v n]
  (.visitIntInsn v &op n))

(defops [newarray]
  "Make a new primitive array of operand type `atype`."
  [v atype]
  (.visitIntInsn v &op (util/array-type atype)))

(defops [goto jsr]
  "Unconditionally jump to offset `label`."
  [v label]
  (.visitJumpInsn v &op (util/label-from label)))

(defops [ifnull ifnonnull
         ifeq ifne, ifge ifgt, ifle iflt
         if_acmpeq if_acmpne
         if_icmpeq if_icmpne
         if_icmpge if_icmpgt
         if_icmple if_icmplt]
  "Conditionally jump to offset `label`."
  [v label]
  (.visitJumpInsn v &op (util/label-from label)))

(defops [ldc]
  "Load constant null, int, float, String, Type, Handle, or
  ConstantDynamic value `x`."
  [v x]
  (let [x (if (instance? Boolean x)
            (if x 1 0)
            x)]
    (cond
      (integer? x)
      (let [n (int x)]
        (case n
          -1 (.visitInsn v Opcodes/ICONST_M1)
          0 (.visitInsn v Opcodes/ICONST_0)
          1 (.visitInsn v Opcodes/ICONST_1)
          2 (.visitInsn v Opcodes/ICONST_2)
          3 (.visitInsn v Opcodes/ICONST_3)
          4 (.visitInsn v Opcodes/ICONST_4)
          5 (.visitInsn v Opcodes/ICONST_5)
          (cond
            (<= Byte/MIN_VALUE n Byte/MAX_VALUE)
            (.visitIntInsn v Opcodes/BIPUSH n)
            (<= Short/MIN_VALUE n Short/MAX_VALUE)
            (.visitIntInsn v Opcodes/SIPUSH n)
            :else
            (.visitLdcInsn v n))))

      (float? x)
      (let [n (float x)]
        (condp = n
          0.0 (.visitInsn v Opcodes/FCONST_0)
          1.0 (.visitInsn v Opcodes/FCONST_1)
          2.0 (.visitInsn v Opcodes/FCONST_2)
          (.visitLdcInsn v n)))

      (nil? x)
      (.visitInsn v Opcodes/ACONST_NULL)

      (or (instance? Handle x) (instance? ConstantDynamic x) (string? x))
      (.visitLdcInsn v x)

      :else
      (.visitLdcInsn v (util/type x)))))

(defops [invokeinterface invokespecial
         invokestatic invokevirtual]
  "Call method named `mname` of class `cls` with the method arguments
  and return type given by `desc`.

  If the type descriptor is not provided or is a numeric arity, it will
  be determined through reflection. In this case, the class must be
  given as a string or Class object."
  ([v cls mname] (&fn v cls mname -1))
  ([v cls mname desc-or-arity]
   (let [mname (util/method-name mname)
         desc (if (number? desc-or-arity)
                (let [cls (if (class? cls)
                            cls
                            (Class/forName cls))
                      arity (long desc-or-arity)
                      static? (= Opcodes/INVOKESTATIC &op)
                      descs (for [^Method m (.getMethods ^Class cls)
                                  :when (and (= mname (.getName m))
                                             (= static?
                                                (Modifier/isStatic (.getModifiers m))))
                                  :let [desc (.getParameterTypes m)]
                                  :when (or (neg? arity)
                                            (== arity (count desc)))]
                              (concat desc
                                      [(.getReturnType m)]))]
                  (if (== 1 (count descs))
                    (first descs)
                    (let [nstr (if (zero? (count descs)) "no" "multiple")
                          type (if static? "static" "instance")
                          plural (if (= nstr "no") "" "s")
                          msg (format "%s %s method%s found with name '%s'%s"
                                      nstr type plural mname
                                      (if (neg? arity)
                                        ""
                                        (str " with arity " arity)))]
                      (throw (ex-info msg {:candidates descs})))))
                desc-or-arity)]
     (.visitMethodInsn v &op (util/class-desc cls)
                       mname (util/method-desc desc)))))

(defops [invokedynamic]
  "Call bootstrap method `boot` to return callsite for method `mname`
  with arguments and return type given by `desc`. Note that this
  instruction requires version 7 bytecode and above.

  The bootstrap method can be a ASM Handle (see `insn.util`) or an op
  sequence of the form specified by the invokeX instructions.

  An optional seq of constant arguments `args` may be given and each
  must be either an Integer, Float, Long, Double, String, ASM Type, ASM
  Handle, or ASM ConstantDynamic object. These are passed to the
  bootstrap method."
  ([v mname desc boot] (&fn v mname desc boot []))
  ([v mname desc boot args]
   (let [boot (if (sequential? boot)
                (apply util/handle boot)
                boot)]
     (.visitInvokeDynamicInsn v (util/method-name mname) (util/method-desc desc)
                              boot (object-array args)))))

(defops [multianewarray]
  "Make a new array of type `atype` with `dims` dimensions."
  [v atype dims]
  (.visitMultiANewArrayInsn v (util/type-desc atype) dims))

(defops [tableswitch]
  "Constant-time index of int table between `imin` and `imax` inclusive,
  jumping to corresponding label in `labels`. If the value 'off' is not
  within range, jump to the `default` label. Otherwise, jump to the
  label index given by: (- (- imax imin) (- imax off))"
  [v imin imax default labels]
  (.visitTableSwitchInsn v imin imax (util/label-from default)
                         (util/label-array labels)))

(defops [anewarray checkcast
         instanceof new]
  "Special type instruction."
  [v stype]
  (.visitTypeInsn v &op (util/special-desc stype)))

(defops [aload astore
         dload dstore
         fload fstore
         iload istore
         lload lstore]
  "Push or write local variable at index `idx`."
  [v idx]
  (.visitVarInsn v &op idx))

(defops [ret]
  "Continue from address given by local variable at index `idx`."
  [v idx]
  (.visitVarInsn v &op idx))

(defops
  [nop
   aconst_null, iconst_m1 iconst_0 iconst_1 iconst_2 iconst_3 iconst_4 iconst_5
   lconst_0 lconst_1, fconst_0 fconst_1 fconst_2, dconst_0 dconst_1
   iaload laload faload daload aaload baload caload saload
   iastore lastore fastore dastore aastore bastore castore sastore
   pop pop2, dup dup_x1 dup_x2 dup2 dup2_x1 dup2_x2, swap
   iadd ladd fadd dadd, isub lsub fsub dsub
   imul lmul fmul dmul, idiv ldiv fdiv ddiv
   irem lrem frem drem, ineg lneg fneg dneg
   ishl lshl ishr lshr iushr lushr, iand land, ior lor, ixor lxor
   i2l i2f i2d, l2i l2f l2d, f2i f2l f2d, d2i d2l d2f, i2b i2c i2s
   lcmp fcmpl fcmpg dcmpl dcmpg
   ireturn lreturn freturn dreturn areturn return
   arraylength athrow monitorenter monitorexit]
  "Single-byte opcode."
  [v]
  (.visitInsn v &op))

;;;

(defn ldc2
  "Load constant long or double value `x`."
  [^MethodVisitor v x]
  (cond
    (integer? x)
    (let [n (long x)]
      (case n
        0 (.visitInsn v Opcodes/LCONST_0)
        1 (.visitInsn v Opcodes/LCONST_1)
        (.visitLdcInsn v n)))
    (float? x)
    (let [n (double x)]
      (condp = n
        0.0 (.visitInsn v Opcodes/DCONST_0)
        1.0 (.visitInsn v Opcodes/DCONST_1)
        (.visitLdcInsn v n)))
    :else
    (throw (IllegalArgumentException. "ldc2 requires a number"))))

(defn line-number
  "Add a reference to source line `n` starting at `label`. If the label
  has not yet been visited (marked), an error will be thrown."
  [^MethodVisitor v ^long n label]
  (.visitLineNumber v n (util/label-from label)))

(defn local-variable
  "Add a reference to a local variable `vname` of type `vtype` with
  index number `idx`. The variable scope is demarcated by labels `start`
  and `end`. If either label has not yet been visited (marked), an
  error will be thrown."
  [^MethodVisitor v vname vtype start end idx]
  (.visitLocalVariable v (name vname) (util/type-desc vtype) nil
                       (util/label-from start) (util/label-from end) idx))

(defn lookupswitch*
  "Jump to a corresponding label given in `tlabels` by an int table
  lookup of the pre-sorted `tkeys` (as per `zipmap`.) If no mapping is
  found, jump to the `default` label."
  [^MethodVisitor v default tkeys tlabels]
  (.visitLookupSwitchInsn v (util/label-from default) (int-array tkeys)
                          (util/label-array tlabels)))

(defn lookupswitch
  "Like `lookupswitch*` with the keys and labels given as a map."
  [v default m]
  (let [[tkeys tlabels] ((juxt identity (partial map m))
                         (sort (keys m)))]
    (lookupswitch* v default tkeys tlabels)))

(defn trycatch
  "Mark region between `start` and `end` labels as protected by label
  `handler` against throwable `etype`."
  [^MethodVisitor v start end handler etype]
  (let [[start end handler] (map util/label-from [start end handler])]
    (.visitTryCatchBlock v start end handler (when-not (nil? etype)
                                               (util/class-desc etype)))))

(defn visit-label!
  "Set label offset to current visitor position."
  [^MethodVisitor v label]
  (.visitLabel v (util/label-from label)))

;;;

(def ^{:doc "Alias for new since new is a special form in clojure."
       :arglists '([v])}
  anew new)

(def ^{:doc "Alias for visit-label!."
       :arglists '([v label])}
  mark visit-label!)

(def ^{:doc "Alias for pop since clojure.core also defines pop."
       :arglists '([v])}
  pop1 pop)

(def-op-method anew)
(def-op-method ldc2)
(def-op-method line-number)
(def-op-method local-variable)
(def-op-method lookupswitch)
(def-op-method mark)
(def-op-method pop1)
(def-op-method trycatch)

(defn ^:internal op-seq
  "Return a flattened sequence of ops."
  [xs]
  (let [op? (comp keyword? first)
        go (fn go [[x & xs]]
             (lazy-seq
              (cond
                (op? x)
                (cons x (go xs))
                (seq x)
                (concat (go x) (go xs))
                (seq xs)
                (go xs))))]
    (go xs)))

(defn compile
  "Compile a sequence of op seqs to a fn that accepts an ASM
  MethodVisitor to emit method bytecode.

  Nested bytecode sequences will be flattened, and nils removed."
  [ops]
  (let [ops (mapv -op (op-seq ops))]
    (fn [v]
      (doseq [op ops]
        (apply (::fn op) v (::args op))))))
