(defproject redis "0.1.0-SNAPSHOT"
  :description "A barebones implementation of a Redis server"
  :url "http://github.com/codecrafters-io/redis-starter-clojure"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :main ^:skip-aot redis.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
