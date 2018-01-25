(when *ns* (require '[insn.namespace :as ns]))

(ns/with-imports insn.module
  "Generate module definitions. Note that loading this namespace
  currently (as of Clojure 1.9) requires an external ASM (>= 6.0) since
  the ASM bundled with Clojure is too old."
  (:require [insn.util :as util]))

(def ^{:doc "The bytecode version to use for modules if unspecified."
       :dynamic true}
  *bytecode-version* 9)

(defn visit
  "Generate a module definition from the given map. Returns a map of the
  module's class :name (always \"module-info\") and :bytes. Options:

    :id        name of the module (required). May be given as a simple
               name or as a tuple of [name module-version].

    :flags     seq of module flags (e.g., :open). See `insn.util`.

    :exports   sequence of exported packages. Each export may be given
               as a fully-qualified package name, or a map with :package
               (required), :flags, and :modules keys.

    :opens     sequence of opened packages. Accepts the same element
               values as :exports.

    :provides  map of services provided by this module of the form
               {service impl}, where impl is either a single type or a
               sequence of types that implement the service.

    :requires  sequence of modules required by this module. Each
               dependency may be given as a simple name or a map with
               :id (required), and :flags. Like the top-level :id
               option, :id may be given as a tuple.

    :uses      sequence of service types used by the module.

    :version   bytecode version given as an integer. Must be >= 9.
               Defaults to 9."
  [m]
  (let [cls "module-info"
        version (:version m *bytecode-version*)
        bversion (util/check-valid "version" util/version? version)
        cv (doto (ClassWriter. 0)
             (.visit bversion (util/flags [:module])
                     cls nil nil nil))

        spec (fn [x]
               (let [[id ver] (if (coll? x) x [x])
                     ver (and ver (if (number? ver)
                                    (str ver)
                                    (name ver)))]
                 [(name id) ver]))

        [mname mver] (spec (:id m))
        flags (util/flags (:flags m #{}))
        v (.visitModule cv mname flags mver)

        reqs (for [x (:requires m)
                   :let [r (if (map? x) x {:id [x]})]]
               (update r :id spec))
        base? (some (comp #{"java.base"} first :id) reqs)]

    (doseq [x (:exports m)]
      (if (map? x)
        (.visitExport v (util/class-desc (name (:package x)))
                      (util/flags (:flags x #{}))
                      (into-array String (map name (:modules x))))
        (.visitExport v (util/class-desc (name x)) 0 nil)))

    (doseq [x (:opens m)]
      (if (map? x)
        (.visitOpen v (util/class-desc (name (:package x)))
                    (util/flags (:flags x #{}))
                    (into-array String (map name (:modules x))))
        (.visitOpen v (util/class-desc (name x)) 0 nil)))

    (doseq [[x xs] (:provides m)]
      (let [impls (map util/class-desc (if (coll? xs) xs [xs]))]
        (.visitProvide v (util/class-desc x)
                       (into-array String impls))))

    (when-not base?
      (.visitRequire v "java.base" (util/flags [:mandated]) nil))
    (doseq [{[nm ver] :id :as r} reqs]
      (.visitRequire v (name nm) (util/flags (:flags r #{})) ver))

    (doseq [x (:uses m)]
      (.visitUse v (util/class-desc x)))

    (.visitEnd v)
    (.visitEnd cv)
    {:name cls, :bytes (.toByteArray cv)}))

(def ^{:arglists '([m])
       :doc "Alias of `visit`."}
  generate visit)
