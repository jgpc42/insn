(if (or (System/getProperty "insn.objectweb-asm")
        (resolve 'insn.objectweb-asm/enable))
  (import
   [org.objectweb.asm
    AnnotationVisitor
    ClassVisitor
    ClassWriter
    FieldVisitor
    Handle
    Label
    MethodVisitor
    Opcodes
    Type])
  (import
   [clojure.asm
    AnnotationVisitor
    ClassVisitor
    ClassWriter
    FieldVisitor
    Handle
    Label
    MethodVisitor
    Opcodes
    Type]))

(import
 [clojure.lang
  AFunction
  Compiler$FnMethod
  DynamicClassLoader
  Keyword
  RT
  Sequential
  Symbol]

 [java.io
  File
  FileOutputStream]

 [java.lang.annotation
  Retention
  RetentionPolicy])
