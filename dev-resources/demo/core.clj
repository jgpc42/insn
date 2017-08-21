(ns demo.core
  (:require [insn.clojure :as bc]
            [insn.op :as op])
  (:import [clojure.lang RT ISeq]))

(def ^{:arglists '(^long [^long x])}
  incr (bc/fn ^long [^long x]
         [[:lload 1]
          [:ldc2 1]
          [:ladd]
          [:lreturn]]))

(bc/defn add
  (^long [^long x]
   [[:aload 0]
    [:lload 1]
    [:ldc2 0]
    [:invokevirtual :this "invokePrim" [:long :long :long]]
    [:lreturn]])
  (^long [^long x ^long y]
   [[:lload 1]
    [:lload 3]
    [:ladd]
    [:lreturn]])
  ([a b & more]
   [[:aload 0]
    [:aload 1] [:invokestatic RT "longCast" [Object :long]]
    [:aload 2] [:invokestatic RT "longCast" [Object :long]]
    [:invokevirtual :this "invokePrim" [:long :long :long]]
    [:lstore 4]
    [:mark :LOOP]
    [:aload 3] [:invokestatic RT "seq" [Object ISeq]]
    [:ifnull :DONE]
    [:aload 0]
    [:lload 4]
    [:aload 3] [:invokestatic RT "first" [Object Object]]
    [:invokestatic RT "longCast" [Object :long]]
    [:invokevirtual :this "invokePrim" [:long :long :long]]
    [:lstore 4]
    [:aload 3] [:invokestatic RT "next" [Object ISeq]]
    [:astore 3] [:goto :LOOP]
    [:mark :DONE]
    [:lload 4]
    [:invokestatic Long "valueOf" [:long Long]]
    [:areturn]]))

(bc/defn calc
  "Calculate some formula of `x`. If `y` is null, do nothing."
  ^double [^long x y]
  (fn [v]
    (let [op #(do (op/ldc2 %2 %3) (% %2))
          [+ - *] (map #(partial op %)
                       [op/ladd op/lsub op/lmul])]
      (doto v
        (op/aload 3)
        (op/ifnull :RET)
        (op/lload 1)
        (+ 1) (* 2) (- 4)
        (op/lstore 1)
        (op/mark :RET)
        (op/lload 1)
        (op/l2d)
        (op/dreturn)))))
