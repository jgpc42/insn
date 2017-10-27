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



[mage]:    https://github.com/nasser/mage
[semver]:  http://semver.org/
