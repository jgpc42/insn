## 0.3.1 (2018-04-27)

* make requiring `insn.objectweb-asm` a no-op

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



[asm]:      http://asm.ow2.org/history.html
[mage]:     https://github.com/nasser/mage
[modules]:  https://github.com/jgpc42/insn/wiki/Java-9-Modules
[semver]:   http://semver.org/
