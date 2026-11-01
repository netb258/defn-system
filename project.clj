(defproject DEFN-System "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  ;; :plugins [[cider/cider-nrepl "0.49.0"]]
  ;; NOTE: The custom Z80 CPU library can also be accessed through JitPack:
  ;; :repositories [["jitpack" "https://jitpack.io"]]
  :resource-paths ["local-libs/Z80Processor-2.0.2.jar"]
  :dependencies [[org.clojure/clojure "1.11.3"]
                 ;; NOTE: The custom Z80 CPU library can also be accessed through JitPack:
                 ;; [com.github.netb258/custom-Z80Processor "main-SNAPSHOT"]
                 [com.esotericsoftware/kryo "5.6.0"]
                 [quil "2.7.1"]]
  :main ^:skip-aot z80.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
