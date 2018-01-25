(defproject insn "0.2.1"
  :description "Functional JVM bytecode generation for Clojure."
  :url "https://github.com/jgpc42/insn"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]]

  :min-lein-version "2.0.0"

  :aliases {"run-test" ["with-profile" "+test" "run" "-m" "insn.test"]
            "run-test-aot" ["with-profile" "+test"
                            "do" "compile" "insn.test,"
                            "run" "-m" "insn.test,"
                            "clean"]
            "test" ["do" "clean,"
                    "run-test,"
                    "run-test-aot,"
                    "with-profile" "+asm5.2" "run-test,"
                    "with-profile" "+asm5.2" "run-test-aot,"
                    "with-profile" "+asm6.0" "run-test,"
                    "with-profile" "+asm6.0" "run-test-aot"]
            "test-all" ["do" "test,"
                        "with-profile" "+1.9" "test,"
                        "with-profile" "+1.7" "test"]}

  :profiles
  {:1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
   :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}
   :dev {:dependencies [[javax.xml.ws/jaxws-api "2.3.0"]]}
   :asm5.2 {:dependencies [[org.ow2.asm/asm "5.2"]]
            :jvm-opts ["-Dinsn.objectweb-asm"]}
   :asm6.0 {:dependencies [[org.ow2.asm/asm "6.0"]]
            :jvm-opts ["-Dinsn.objectweb-asm"]}
   :repl {:source-paths ["dev"]}})
