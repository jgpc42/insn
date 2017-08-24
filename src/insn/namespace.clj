(ns ^:no-doc insn.namespace
  "Namespace utils.")

(def ^:private asm-import-decl
  (if (or (System/getProperty "insn.objectweb-asm")
          (find-ns 'insn.objectweb-asm))
    '[org.objectweb.asm
      AnnotationVisitor
      ClassVisitor
      ClassWriter
      FieldVisitor
      Handle
      Label
      MethodVisitor
      Opcodes
      Type]
    '[clojure.asm
      AnnotationVisitor
      ClassVisitor
      ClassWriter
      FieldVisitor
      Handle
      Label
      MethodVisitor
      Opcodes
      Type]))

(def ^:private import-decls
  (list asm-import-decl
        '[clojure.lang
          AFunction
          Compiler$FnMethod
          DynamicClassLoader
          Keyword
          RestFn
          RT
          Sequential
          Symbol]
        '[java.io
          File
          FileOutputStream]
        '[java.lang.annotation
          Retention
          RetentionPolicy]))

(defmacro ^:internal with-imports
  "The `clojure.core/ns` macro with auto-imports used by `insn`."
  [& forms]
  (concat (list* `ns forms)
          [(list* :import import-decls)]))
