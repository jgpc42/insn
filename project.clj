(defproject insn "0.3.2-SNAPSHOT"
  :description "Functional JVM bytecode generation for Clojure."
  :url "https://github.com/jgpc42/insn"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.ow2.asm/asm "7.0"]]

  :min-lein-version "2.0.0"

  :aliases {"test" ["with-profile" "+test" "run" "-m" "insn.test"]
            "test-all" ["do" "test,"
                        "with-profile" "+1.10" "test,"
                        "with-profile" "+1.9" "test,"
                        "with-profile" "+1.7" "test"]}

  :profiles
  {:1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
   :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}
   :1.10 {:dependencies [[org.clojure/clojure "1.10.0"]]}
   :dev {:dependencies [[javax.xml.ws/jaxws-api "2.3.0"]]}
   :repl {:source-paths ["dev"]}})
