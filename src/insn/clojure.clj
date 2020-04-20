(ns insn.clojure
  "Generate Clojure fns whose bodies are specified as bytecode."
  (:refer-clojure :exclude [defn defn- fn])
  (:require [insn.core :as core]
            [insn.op :as op]
            [insn.util :as util])
  (:import [clojure.lang AFunction Compiler$FnMethod RestFn RT]))

(defmacro ^:private check [expr msg]
  `(when-not ~expr
     (throw (RuntimeException. ~msg))))

(clojure.core/defn- fn-decls [args]
  (let [decls (if (seq? (first args)), args [args])
        [vsig more] (filter #(some #{'&} %) (map first decls))
        nvargs (dec (count vsig))
        vparam (second (split-with #(not= % '&) vsig))
        vprims (->> vsig (map (comp :tag meta)) (filter '#{long double}) seq)
        arities (map (comp count first) decls)]

    (check (nil? more) "can't have multiple variadic overloads")
    (check (apply distinct? arities) "can't have two overloads with same arity")

    (when vsig
      (check (not (== (count vparam) 1)) "missing variadic parameter after '&'")
      (check (== (count vparam) 2) "invalid extra variadic parameter")
      (check (nil? vprims) "variadic overload cannot take or yield primitives"))

    (doseq [[sig body & more] decls]
      (check (every? symbol? sig) "bytecode fn arglist should be symbols only")
      (check (nil? more) "bytecode fn body should be single expression")
      (when (and vsig (not= sig vsig))
        (check (< (count sig) nvargs) "variadic overload must have greatest arity")))

    decls))

(clojure.core/defn- fn-methods [sig body]
  (let [vararg? (some #{'&} sig)
        nargs (long (if vararg? (dec (count sig)) (count sig)))
        objs (vec (repeat (inc nargs) `Object))
        tag #(-> % meta :tag ('{long :long, double :double} `Object))
        prims (and (not vararg?)
                   (conj (mapv tag sig) (tag sig)))
        body (if (vector? body) `(op/compile ~body) body)]
    (cond
      (and prims (not= prims objs))
      [{:name "invokePrim", :desc prims, :emit body}
       {:name "invoke", :desc objs
        :emit [[:aload 0]
               (vec
                (mapcat
                 (clojure.core/fn [i arg]
                   [[:aload i]
                    (condp = arg
                      :long [:invokestatic `RT "longCast" [`Object :long]]
                      :double [:invokestatic `RT "doubleCast" [`Object :double]]
                      nil)
                    [:ldc nil]
                    [:astore i]])
                 (next (range))
                 (butlast prims)))
               [:invokevirtual :this "invokePrim" prims]
               (condp = (last prims)
                 :long [:invokestatic `Long "valueOf" [:long `Long]]
                 :double [:invokestatic `Double "valueOf" [:double `Double]]
                 nil)
               [:areturn]]}]
      vararg?
      [{:name "getRequiredArity", :desc [:int]
        :emit [[:ldc (dec nargs)]
               [:ireturn]]}
       {:name "doInvoke", :desc objs, :emit body}]
      :else
      [{:name "invoke", :desc objs, :emit body}])))

;;;

(defmacro fn
  "Bytecode version of `clojure.core/fn`. The optional fn name and
  argument vector(s) are only for documentation, primitive type
  hinting, or to indicate a variadic fn.

  The fn body expressions are replaced by a single expression that
  should evaluate to a fn that takes an ASM MethodVisitor. As a
  shortcut, if the compile-time type of the body is a vector, it will be
  compiled via `insn.op/compile`."
  [& args]
  (let [[fname args] (util/optional symbol? args 'insn-fn)
        decls (fn-decls args)
        sigs (map first decls)
        super (if (some (partial some #{'&}) sigs) `RestFn `AFunction)
        ifaces (for [sig sigs
                     :let [iface (Compiler$FnMethod/primInterface sig)]
                     :when iface]
                 (symbol iface))]
    `(core/new-instance
      {:name '~(gensym fname)
       :super ~super
       :interfaces ~(vec ifaces)
       :methods [~@(mapcat (partial apply fn-methods) decls)]})))

(defmacro defn
  "Bytecode version of `clojure.core/defn`. See `fn`.

  If the fn name metadata or an attr-map contains :insn/version, it
  specifies the bytecode version to generate.

  Note that the pre/post condition map is not supported."
  [fname & args]
  (let [[doc args] (util/optional string? args)
        [pre args] (util/optional map? args)
        [decls args] (if (seq? (first args))
                       (split-with seq? args)
                       [(list args) nil])
        [post more] (util/optional map? args)
        version (some identity (map :insn/version [(meta fname) pre post]))]
    (check (nil? more) (str "trailing defn data: " (pr-str (first more))))
    `(do
       (def ~fname (binding [~@(when version
                                 `[[core/*bytecode-version* ~version]])]
                     (fn ~fname ~@decls)))
       (doto #'~fname
         (alter-meta! merge {:arglists '~(map first decls)
                             :doc ~doc}
                      ~pre ~post)))))

(defmacro defn-
  "Bytecode version of `clojure.core/defn-`. See `defn`."
  [fname & args]
  (check (symbol? fname) (str "invalid defn- name: " (pr-str fname)))
  (list* `defn (vary-meta fname assoc :private true) args))
