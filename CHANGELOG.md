## 0.5.5-SNAPSHOT (20XX-XX-XX)

* Upgrade to [ASM 9.6][asm]

## 0.5.4 (2022-04-14)

* BUGFIX: default `iface` value for method instructions should be false *except* for `invokeinterface` ([#12][issue12])

## 0.5.3 (2022-04-12)

* Upgrade to [ASM 9.3][asm]
* Explicit support for JDK 15, 16, 17, and 18
* Explicit support for Clojure 1.11
* Support passing boolean last argument for method instructions to specify interface ([#11][issue11])

## 0.5.2 (2021-03-21)

* Support giving `:source` file name and `:debug` information with `insn.core/visit` ([#9][issue9])

## 0.5.1 (2021-02-12)

* Load some ASM Opcodes and classes dynamically to support using library with older ASM versions
* BUGFIX: allow `nil` as the exception type of `:trycatch` to enable proper `try/finally` support ([#7][issue7])

## 0.5.0 (2020-10-07)

* Upgrade to [ASM 9.0][asm]
* Explicit support for JDK 12, 13, and 14
* Add `deps.edn` file
* Support for generic type information interop via the `:signature` key and new protocol `TypeSig`

## 0.4.0 (2018-12-17)

* Upgrade to [ASM 7.0][asm]
* Support JDK 11
* Drop explicit support for Java 6 and 7
* Add support for generating constant dynamic values
* Improve error message when `:this` and `:super` class name proxies are unbound
* BUGFIX: correct typo mapping `:deprecated` to `ACC_BRIDGE` instead of `ACC_DEPRECATED`

## 0.3.1 (2018-04-27)

* Make requiring `insn.objectweb-asm` a no-op

## 0.3.0 (2018-04-27)

* Require external ASM dependency to simplify code and support latest bytecode features
* Generate version 1.6, not 1.7, bytecode by default for JDK 6

## 0.2.1 (2018-01-25)

* Add support for generating [modules][modules] (requires ASM version >= [`6.0`][asm])
* Add support for version 9 bytecode and specifying bytecode `:version` as an integer
* BUGFIX: arity exception when modifier `:flags` were explicitly `nil` under rare circumstances

## 0.2.0 (2017-10-27)

* Determine types for field instructions automatically when not specified
* Determine type descriptors for invoke instructions automatically when not specified
* VERSIONING: (finally) increment the [*minor*][semver] library version

## 0.1.4 (2017-09-07)

* Allow passing constructor arguments to `new-instance`
* Flatten nested bytecode op sequences automatically (like [Mage][mage])

## 0.1.3 (2017-08-25)

* AOT compilation support
* Allow `:this` and `:super` in type descriptors
* Support bytecode line number and local variable metadata
* Auto cast numeric static field default values
* Eagerly clear GC references in primitive bytecode fns

## 0.1.2 (2017-08-21)

* Support for variadic bytecode fns
* Allow non-string class field names
* Allow specifing bytecode version in metadata for bytecode `defn` and `defn-`
* Support the `:mandated` ASM flag

## 0.1.1 (2017-08-17)

* Allow empty sequences to specify default modifier flags
* Allow non-keyword return type specifier for constructor method descriptors



[asm]:      https://asm.ow2.io/versions.html
[issue7]:   https://github.com/jgpc42/insn/issues/7
[issue9]:   https://github.com/jgpc42/insn/issues/9
[issue11]:  https://github.com/jgpc42/insn/issues/11
[issue12]:  https://github.com/jgpc42/insn/issues/12
[mage]:     https://github.com/nasser/mage
[modules]:  https://github.com/jgpc42/insn/wiki/Java-9-Modules
[semver]:   http://semver.org/
