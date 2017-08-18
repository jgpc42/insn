(ns insn.clojure
  "Generate Clojure fns whose bodies are specified as bytecode."
  (:refer-clojure :exclude [defn defn- fn])
  (:require [insn.core :as core]
            [insn.op :as op]
            [insn.util :as util])
  (:load "imports"))

(defmacro fn
  "Bytecode version of `clojure.core/fn`. The optional fn name and
  argument vector(s) are only for documentation and primitive type
  hinting.

  The fn body expressions are replaced by a single expression that
  should evaluate to a fn that takes an ASM MethodVisitor. As a
  shortcut, if the compile-time type of the body is a vector, it will be
  compiled via `op/compile`."
  [& args]
  (let [[fname args] (util/optional symbol? args 'insn-fn)
        decls (if (seq? (first args)), args [args])
        ifaces (reduce conj {}
                       (for [[sig] decls
                             :let [iface (Compiler$FnMethod/primInterface sig)]
                             :when iface]
                         [sig (symbol iface)]))
        methods (clojure.core/fn [[sig body & more]]
                  (assert (nil? more) "fn body should be a single expression")
                  (let [objs (vec (repeat (inc (count sig)) `Object))
                        prims (when-let [iface (get ifaces sig)]
                                (mapv {\L :long, \D :double, \O `Object}
                                      (last (.split (str iface) "\\$"))))
                        body (if (vector? body) `(op/compile ~body) body)]
                    (if prims
                      [{:name "invokePrim", :desc prims, :emit body}
                       {:name "invoke", :desc objs
                        :emit `[[:aload 0]
                                ~@(mapcat
                                   (clojure.core/fn [i arg]
                                     `[[:aload ~i]
                                       ~@(condp = arg
                                           :long [[:invokestatic `RT "longCast" [`Object :long]]]
                                           :double [[:invokestatic `RT "doubleCast" [`Object :double]]]
                                           nil)])
                                   (next (range))
                                   (butlast prims))
                                [:invokevirtual :this "invokePrim" ~prims]
                                ~@(condp = (last prims)
                                    :long [[:invokestatic `Long "valueOf" [:long `Long]]]
                                    :double [[:invokestatic `Double "valueOf" [:double `Double]]]
                                    nil)
                                [:areturn]]}]
                      [{:name "invoke", :desc objs, :emit body}])))]
    `(core/new-instance
      {:name '~(gensym fname)
       :super clojure.lang.AFunction
       :interfaces ~(vec (vals ifaces))
       :methods [~@(mapcat methods decls)]})))

(defmacro defn
  "Bytecode version of `clojure.core/defn`. See `fn`.

  Note that the pre/post condition map is not supported."
  [fname & args]
  (let [[doc args] (util/optional string? args)
        [pre args] (util/optional map? args)
        [decls args] (if (seq? (first args))
                       (split-with seq? args)
                       [(list args) nil])
        [post] (util/optional map? args)]
    `(do
       (def ~fname (fn ~fname ~@decls))
       (doto #'~fname
         (alter-meta! merge {:arglists '~(map first decls)
                             :doc ~doc}
                      ~pre ~post)))))

(defmacro defn-
  "Bytecode version of `clojure.core/defn-`. See `defn`."
  [fname & args]
  (assert (symbol? fname) fname)
  (list* `defn (vary-meta fname assoc :private true) args))
