(defproject redis "0.1.0-SNAPSHOT"
  :description "A barebones implementation of a Redis server"
  :url "http://github.com/codecrafters-io/redis-starter-clojure"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/tools.cli "1.1.230"]
                 [aleph "0.8.3"]]

  :main ^:skip-aot redis.core
  :target-path "/tmp/codecrafters-redis-target/%s"
  :clean-targets ^{:protect false} ["/tmp/codecrafters-redis-target"]
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
