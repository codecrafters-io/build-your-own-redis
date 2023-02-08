(ns redis.core
  (:gen-class))

(require '[clojure.java.io :as io])
(import '[java.net ServerSocket])

(defn receive-message
  "Read a line of textual data from the given socket"
  [socket]
  (.readLine (io/reader socket)))

(defn send-message
  "Send the given string message out over the given socket"
  [socket msg]
  (let [writer (io/writer socket)]
    (.write writer msg)
    (.flush writer)))

(defn serve [port handler]
  (with-open [server-sock (ServerSocket. port)
              sock (.accept server-sock)]
    (. server-sock (setReuseAddress true))
    (let [msg-in (receive-message sock)
          msg-out (handler msg-in)]
      (send-message sock msg-out))))

(defn handler
  [& args]
  "TODO: <respond-with-output>")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (serve 6379 handler)
  )
