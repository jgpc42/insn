(defproject insn "0.2.0"
  :description "Functional JVM bytecode generation for Clojure."
  :url "https://github.com/jgpc42/insn"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]]

  :min-lein-version "2.0.0"

  :aliases {"run-test" ["with-profile" "+test" "run" "-m" "insn.test"]
            "run-test-compiled" ["with-profile" "+test"
                                 "do" "compile" "insn.test,"
                                 "run" "-m" "insn.test,"
                                 "clean"]
            "test" ["do" "clean,"
                    "run-test,"
                    "run-test-compiled,"
                    "with-profile" "+external-asm" "run-test,"
                    "with-profile" "+external-asm" "run-test-compiled"]
            "test-all" ["do" "test,"
                        "with-profile" "+1.9" "test,"
                        "with-profile" "+1.7" "test"]}

  :profiles
  {:1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
   :1.9 {:dependencies [[org.clojure/clojure "1.9.0-beta3"]]}
   :external-asm {:dependencies [[org.ow2.asm/asm "5.2"]]
                  :jvm-opts ["-Dinsn.objectweb-asm"]}
   :repl {:source-paths ["dev"]}})
