(ns demo.core
  (:require [insn.clojure :as bc]
            [insn.op :as op]))

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
    [:lreturn]]))

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
