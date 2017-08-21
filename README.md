[![Clojars Project](https://img.shields.io/clojars/v/insn.svg)](https://clojars.org/insn)

### Dependency information

Leiningen

``` clojure
[insn "0.1.1"]
```

Maven

``` xml
<dependency>
  <groupId>insn</groupId>
  <artifactId>insn</artifactId>
  <version>0.1.1</version>
</dependency>
```

### What is it?

This library provides a functional abstraction over [ASM][asm] for generating JVM bytecode. ASM is the library that Clojure itself uses to dynamically compile Clojure code into code that can be run on the JVM. By default, this library uses the bundled ASM that comes with Clojure and thus requires no dependencies aside from Clojure itself.

### Usage

Let's begin by creating a simple class, equivalent to the following Java code.

```java
package my.pkg;

public class Adder {
    public static long VALUE = 42;
    public long add (long n) {
        return VALUE + n;
    }
}
```

The class is specified as a map. The class fields and methods are sequences of maps giving the members of said class.

```clojure
(def class-data
  {:name 'my.pkg.Adder
   :fields [{:flags #{:public :static}, :name "VALUE", :type :long, :value 42}]
   :methods [{:flags #{:public}, :name "add", :desc [:long :long]
              :emit [[:getstatic :this "VALUE" :long]
                     [:lload 1]
                     [:ladd]
                     [:lreturn]]}]})
```

Above, we described in data exactly the same information expressed by the Java code, except the method body was given as a sequence of bytecode instructions. If you aren't fluent in JVM bytecode instruction syntax, I would suggest reading chapter 3 of the excellent tutorial pdf from the ASM [site][pdf].

Now to write the bytecode.

```clojure
(require '[insn.core :as insn])

(def result (insn/visit class-data))
```

The `result` is a map containing the generated classes' packaged-prefixed `:name` and `:bytes`, the latter being a byte array. This information is all you need to give to a ClassLoader to define your class.

For convenience, we can use `insn.core/define` to define the class for us.

```clojure
(def class-object (insn/define class-data)) ;; => my.pkg.Adder
(-> class-object .newInstance (.add 17))    ;; => 59
```

Note that you can also pass `result` to `define`, the class will not be regenerated. Also note, like Java, since we did not define any constructors, a public no-argument constructor that simply calls the superclass constructor was generated for us.

If you are evaluating the code snippets above in the REPL, you can also just do:

```clojure
(.add (my.pkg.Adder.) 17) ;; => 59
```

Since, by default, `define` loads the class using Clojure's own `DynamicClassLoader`, meaning the class will be *first class* to subsequent evaluations in the running Clojure environment.

For more, see the additional examples below, or the [docs][doc]. The [tests][test] are also a good reference.

### Examples

#### Generate an interface and implementation

This example shows how `:emit` can also be a fn that is passed the ASM `MethodVisitor` object to write the method bytecode. The namespace `insn.op` defines a corresponding helper fn for every bytecode instruction, all of which take a `MethodVisitor` as their first argument.

Using a fn is sometimes more convenient than splicing together a seq for complex generation scenarios.

```clojure
(require '[insn.core :as insn]
         '[insn.op :as op])

(insn/define
  {:flags #{:public :interface}
   :name 'my.math.Inc
   :methods [{:flags #{:public :abstract}, :name :inc, :desc [:long :long]}
             {:flags #{:public :abstract}, :name :inc, :desc [:double :double]}]})

(defn make-inc
  "Return an emit fn that will increment the given primitive type."
  [type]
  (let [[load const add ret]
        (if (= type :long)
          [op/lload op/lconst-1 op/ladd op/lreturn]
          [op/dload op/dconst-1 op/dadd op/dreturn])]
    (fn [v]
      (doto v (load 1) const add ret))))

(def ops
  (insn/new-instance
    {:flags #{:public}, :name 'my.math.IncImpl, :interfaces ['my.math.Inc]
     :methods [{:flags #{:public}, :name :inc, :desc [:long :long], :emit (make-inc :long)}
               {:flags #{:public}, :name :inc, :desc [:double :double], :emit (make-inc :double)}]}))

(.inc ops 42)   ;; => 43
(.inc ops 43.0) ;; => 44.0
```

We use the `new-instance` helper to quickly define and instantiate an instance of our class.

#### Generating Clojure vars

The `insn.clojure` namespace provides helpers to create Clojure fns and vars. The fn body expression(s) of the original Clojure versions being instead a single expression suitable for being `:emit`-ed.

```clojure
(require '[insn.clojure :as bc])

(bc/defn my-inc ^long [^long n]
  [[:lload 1]
   [:ldc2 1]
   [:ladd]
   [:lreturn]])

(map my-inc (range 3)) ;; => (1 2 3)
```

Since the argument list is adorned with the proper metadata, the above generates a fn that implements Clojure's `clojure.lang.IFn$LL` interface (indicating primitive support, taking and returning a `long`), and will not box the argument or return type when appropriate.

```clojure
(set! *unchecked-math* :warn-on-boxed)

(inc (my-inc 42)) ;; => 44
```

Note that it helps to have some understanding of how Clojure compiles fns before using these. To learn more, I would suggest using the excellent [no.disassemble][nodis] library to disassemble some Clojure fns.

#### Faster hashcode

Let's say we want a more performant integer array `hashCode` implementation in Clojure (disregarding our knowledge of `java.util.Arrays/hashCode` for the moment). We could simply write a Java helper class, but it's pretty easy to just write it in JVM bytecode.

```clojure
(require '[insn.clojure :as bc])

(bc/defn hashcode ^long [arr]
  [[:aload 1] [:ifnull :L/NULL]                ;; if arr == null then goto NULL
   [:aload 1] [:checkcast [:int]] [:astore 1]  ;; arr = (int[]) arr
   [:aload 1] [:arraylength]      [:istore 2]  ;; len = arr.length
   [:ldc 0]   [:istore 3]                      ;; i = 0
   [:ldc 1]   [:istore 4]                      ;; h = 1
   [:mark :L/LOOP]
   [:iload 2]  [:iload 3] [:if-icmpeq :L/RET]  ;; if len == i then goto RET
   [:ldc 31]   [:iload 4] [:imul]              ;; push h * 31
   [:aload 1]  [:iload 3] [:iaload]            ;; push arr[i]
   [:iadd]     [:istore 4]                     ;; add pushed values, store into h
   [:iinc 3 1] [:goto :L/LOOP]                 ;; i++, goto LOOP
   [:mark :L/RET]
   [:iload 4] [:i2l] [:lreturn]                ;; return h as long
   [:mark :L/NULL]
   [:ldc2 0] [:lreturn]])                      ;; return 0L
```

By my [benchmarks][hcbm], this is about 25% faster than the optimized Clojure equivalent, on par with Java. The reason likely being the additional `l2i` and `i2l` cast instructions at every step of the Clojure version. An `int` is required at each iteration to correctly update the hash value and Clojure's `loop` can only maintain `long`s.

#### Annotations

Just like classes and members, annotations are simply maps. They can be specified for classes, methods, and fields. Provide the `:annotations` key with either a map or sequence of tuples. Method parameter annotations are also supported, specified by a map of parameter indexes to annotations.

``` clojure
(def anns {Deprecated true})

(def my-method
  {:flags [:public], :name "annotated_method", :desc [String :void]
   :emit [#_...] ;; omitted for brevity
   :annotations anns
   :parameter-annotations {0 anns}})
```

Annotation values are processed exactly the same way as in [Clojure][anns].

### Loading constant numbers

When you use `42` or `42.0` in Clojure, you are (in general) using instances of `Long` and `Double`, respectively. However, the `:ldc` instruction will assume you meant to load a primitive `int` or `float` value when said values are used with it. This is to alleviate having to manually cast numbers when `int`s and `float`s are required, like `[:ldc (int 42)]`, which would be tedious and error-prone.

Insn provides the `:ldc2` instruction instead, with the sole purpose of loading constant `long` and `double` values. `:ldc` is used to load all other constant values.

### Bytecode version

By default, this library generates version 1.7 bytecode (the most recent supported by Clojure's ASM). Provide `:version` when visiting your class to override. Note that versions 1.6 and lower do not support all instructions, most notably `invokedynamic`.

``` clojure
(insn/visit (assoc class-data :version 1.5))
```

### How about a higher-level API?

Since method bytecode can be built from simple Clojure sequences and/or function composition, making your own application-specific API is easy.

``` clojure
(require '[insn.core :as insn])
(import [java.io PrintStream])

(defn cat-strs [s & strs]
  (cons [:ldc s]
        (mapcat (fn [s]
                  [[:ldc s]
                   [:invokevirtual String "concat" [String String]]])
                strs)))

(.print
 (insn/new-instance
   {:methods [{:name "print", :desc [:void]
               :emit `[[:getstatic System "out" PrintStream]
                       ~@(apply cat-strs (repeatedly 3 (comp str (partial gensym \-))))
                       [:invokevirtual PrintStream "println" [String :void]]
                       [:return]]}]}))
;; => -3832-3833-3834
```

### Using an external ASM

Simply add a dependency on the ASM version you wish to use to your project, for leiningen, something like `[org.ow2.asm/asm "5.2"]`. Now just require `insn.objectweb-asm` before loading subsequent `insn` namespaces.

``` clojure
(ns my-project.core
  (:require insn.objectweb-asm
            [insn.core :as insn]))
```

Alternatively, you can define the `insn.objectweb-asm` property. For leiningen add `"-Dinsn.objectweb-asm"` to your `:jvm-opts`.

### Running the tests

```bash
lein test
```

### Similar libraries

  - [tools.emitter.jvm](https://github.com/clojure/tools.emitter.jvm)
    * Does not provide an API to all bytecode instructions, only the ones needed for Clojure code compilation.
    * Currently does not support annotations.
  - [mage](https://github.com/nasser/mage) and [magic](https://github.com/nasser/magic)
    * Clojure-CLR only.

### What's with the name?

The name comes from ASM's [MethodVisitor][mvis] class. All instruction-writing methods use the same suffix: *"Insn"*.



[anns]:  https://clojure.org/reference/datatypes#_java_annotation_support
[asm]:   http://asm.ow2.org
[doc]:   https://jgpc42.github.io/insn/doc
[hcbm]:  https://gist.github.com/jgpc42/97fdcf12d4a2977e15b23001f0a612db
[nodis]: https://github.com/gtrak/no.disassemble
[mvis]:  http://asm.ow2.org/asm50/javadoc/user/org/objectweb/asm/MethodVisitor.html
[pdf]:   http://download.forge.objectweb.org/asm/asm4-guide.pdf
[test]:  https://github.com/jgpc42/insn/blob/v0.1.1/test/insn/core_test.clj
