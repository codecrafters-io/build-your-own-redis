(ns redis.core
  (:gen-class))

(require '[clojure.java.io :as io])
(import '[java.net ServerSocket])

(defn send-message
  "Send the given string message out over the given socket"
  [socket msg]
  (let [writer (io/writer socket)]
    (.write writer msg)
    (.flush writer)))

(defn handler
  [& args]
  "+PONG\r\n")

(defn serve [port handler]
  (with-open [server-sock (ServerSocket. port)]
    (. server-sock (setReuseAddress true))

    (with-open [sock (.accept server-sock)]
      (let [in (.getInputStream sock)
            buf (byte-array 1024)]
        (loop []
          (let [n (.read in buf)]
            (when (pos? n)
              (send-message sock (handler))
              (recur))))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (serve 6379 handler)
  )
