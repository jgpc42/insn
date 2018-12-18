[![Clojars Project](https://img.shields.io/clojars/v/insn.svg)](https://clojars.org/insn)
[![Travis CI](https://travis-ci.org/jgpc42/insn.svg?branch=master)](https://travis-ci.org/jgpc42/insn)

### Dependency information

[Leiningen][lein]

``` clojure
[insn "0.4.0"]
```

[tools.deps][deps]

```clojure
{insn {:mvn/version "0.4.0"}}
```

[Maven][maven]

``` xml
<dependency>
  <groupId>insn</groupId>
  <artifactId>insn</artifactId>
  <version>0.4.0</version>
</dependency>
```

Java versions 8 to 11 and Clojure versions 1.7 to 1.10 are currently supported.

### What is it?

This library provides a functional abstraction over [ASM][asm] for generating JVM bytecode. ASM is the library that Clojure itself uses to dynamically compile Clojure code into code that can be run on the JVM.

### Quick start

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

`:emit` can also be a fn that is passed the ASM `MethodVisitor` object to write the method bytecode as shown in [this example][emitfn].

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

### More information

For additional usage examples and topics, see the [wiki][wiki]. For a complete reference, see the [docs][doc].

### Running the tests

```bash
lein test
```

Or, `lein test-all` for all supported Clojure versions.

The tests can also be run against all supported Java versions (via [`docker`][docker]) with:

``` bash
./test-all-jdk.sh
```

### Similar libraries

  - [tools.emitter.jvm](https://github.com/clojure/tools.emitter.jvm)
    * Does not provide a general ASM API.
  - [mage](https://github.com/nasser/mage) and [magic](https://github.com/nasser/magic)
    * Clojure-CLR only.

### Projects using insn

  - [jmh-clojure](https://github.com/jgpc42/jmh-clojure)
    * Clojure bridge to JMH benchmarking via bytecode generation.

### License

Copyright © 2018 Justin Conklin

Distributed under the Eclipse Public License, the same as Clojure.



[asm]:     http://asm.ow2.org
[deps]:    https://github.com/clojure/tools.deps.alpha
[doc]:     https://jgpc42.github.io/insn/doc
[docker]:  https://www.docker.com
[emitfn]:  https://github.com/jgpc42/insn/wiki/Interface-Implementation
[lein]:    http://github.com/technomancy/leiningen
[maven]:   http://maven.apache.org
[pdf]:     http://download.forge.objectweb.org/asm/asm4-guide.pdf
[wiki]:    https://github.com/jgpc42/insn/wiki
