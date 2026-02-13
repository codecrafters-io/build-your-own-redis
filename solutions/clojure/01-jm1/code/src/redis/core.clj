(ns redis.core
  (:import [java.net ServerSocket])
  (:gen-class))

(defn -main [& args]
  (let [server-socket (ServerSocket. 6379)]
    (.setReuseAddress server-socket true)
    (.accept server-socket))
  )
