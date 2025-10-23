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
  (with-open [server-sock (ServerSocket. port)]
    ;; Since the tester restarts your program quite often, setting SO_REUSEADDR
    ;; ensures that we don't run into 'Address already in use' errors
    (. server-sock (setReuseAddress true))

   (with-open [sock (.accept server-sock)]
    (let [msg-in (receive-message sock)
          msg-out (handler msg-in)]
      (send-message sock msg-out)))))

(defn handler
  [& args]
  "TODO: Respond with output")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; You can use print statements as follows for debugging, they'll be visible when running tests.
  (println "Logs from your program will appear here!")
  ;; Uncomment this block to pass the first stage
  ;; (serve 6379 handler)
  )
