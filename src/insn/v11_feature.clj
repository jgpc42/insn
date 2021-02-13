(ns ^:internal ^:no-doc insn.v11-feature
  "This ns contains proxies for fns in other nses."
  (:import [org.objectweb.asm ConstantDynamic]))

(defn constant-dynamic [cname type-desc handle arg-array]
  (ConstantDynamic. cname type-desc handle arg-array))
