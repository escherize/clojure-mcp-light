(babashka.deps/add-deps '{:deps {parinferish/parinferish {:mvn/version "0.8.0"}}})

(ns clojure-mcp-light.socket-eval
  "Socket REPL client with automatic delimiter repair and timeout handling.

   Unlike nREPL, socket REPL is a plain TCP stream: you write forms, the server
   reads, evaluates, and writes results back. No sessions, no message IDs, no
   stdout/stderr split, no interrupt op."
  (:require [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure-mcp-light.delimiter-repair :refer [fix-delimiters]]))

(defn exit!
  "Indirection over `System/exit` so tests can redef it."
  [code]
  (System/exit code))

;; ============================================================================
;; Stdin handling (mirrors nrepl_eval.clj — parallel commands, no shared util)
;; ============================================================================

(defn has-stdin-data?
  "Check if stdin has data available (not a TTY)."
  []
  (try
    (.ready *in*)
    (catch Exception _ false)))

(defn get-code
  "Get code from args or stdin. Arguments take precedence."
  [arguments]
  (cond
    (seq arguments) (first arguments)
    (has-stdin-data?) (slurp *in*)
    :else nil))

;; ============================================================================
;; Socket REPL transport
;; ============================================================================

(defn eval-via-socket
  "Send code to a socket REPL at host:port, stream response to stdout.

   Uses a TCP half-close after writing code. A full close would tear down both
   channels of the socket at once; the server would fail to write its response
   with 'Connection reset by peer'. `shutdownOutput` sends FIN on the write
   side only, so the server sees EOF on input, evaluates, writes results back
   on the still-open read side, then closes from its end.

   On timeout: forcibly closes the socket and exits 124. The running form on
   the server continues until it finishes or errors — socket REPL has no
   interrupt mechanism."
  [{:keys [host port code timeout-ms]
    :or {host "localhost" timeout-ms 120000}}]
  (let [fixed-code (or (fix-delimiters code) code)
        sock (try
               (doto (java.net.Socket.)
                 (.connect (java.net.InetSocketAddress. ^String host ^int port) 5000))
               (catch java.net.ConnectException _
                 (binding [*out* *err*]
                   (println (str "Error: Could not connect to socket REPL at " host ":" port))
                   (println "Is the REPL running?"))
                 (exit! 1))
               (catch java.net.SocketTimeoutException _
                 (binding [*out* *err*]
                   (println (str "Error: Connection timed out to " host ":" port)))
                 (exit! 1)))
        done (promise)
        _timer (future
                 (when (= ::timeout (deref done timeout-ms ::timeout))
                   (binding [*out* *err*]
                     (println (str "Error: Command timed out after " timeout-ms "ms")))
                   (.close sock)
                   (exit! 124)))]
    (let [out (.getOutputStream sock)]
      (.write out (.getBytes fixed-code "UTF-8"))
      (.flush out))
    (.shutdownOutput sock)
    (let [in (.getInputStream sock)
          buf (byte-array 4096)]
      (loop []
        (let [n (.read in buf)]
          (when (pos? n)
            (print (String. buf 0 n "UTF-8"))
            (flush)
            (recur))))
      (.close sock)
      (deliver done :complete))))

;; ============================================================================
;; CLI
;; ============================================================================

(def cli-options
  [["-p" "--port PORT" "Socket REPL port (required)"
    :parse-fn parse-long
    :validate [#(> % 0) "Must be a positive number"]]
   ["-H" "--host HOST" "Host address (default: localhost)"
    :default "localhost"]
   ["-t" "--timeout MILLISECONDS" "Timeout in milliseconds (default: 120000)"
    :default 120000
    :parse-fn parse-long
    :validate [#(> % 0) "Must be a positive number"]]
   ["-h" "--help" "Show this help message"]])

(defn usage [options-summary]
  (str/join \newline
            ["clj-socket-eval - Evaluate Clojure code via socket REPL"
             ""
             "Usage: clj-socket-eval --port PORT CODE"
             "       echo CODE | clj-socket-eval --port PORT"
             "       clj-socket-eval --port PORT <<'EOF' ... EOF"
             ""
             "Options:"
             options-summary
             ""
             "Socket REPL vs nREPL:"
             "  Socket REPL is a plain TCP stream — simpler than nREPL, but has no"
             "  sessions, no stdout/stderr split, and no interrupt. Use clj-nrepl-eval"
             "  if you need those. Delimiter errors are auto-repaired before sending,"
             "  same as clj-nrepl-eval."
             ""
             "Input Methods:"
             "  Prefer heredoc via stdin (<<'EOF' ... EOF) to avoid shell escaping."
             "  Code can also be provided as a command-line argument."
             "  Arguments take precedence over stdin when both are provided."
             ""
             "Examples:"
             "  clj-socket-eval -p 6000 \"(+ 1 2 3)\""
             "  echo \"(+ 1 2 3)\" | clj-socket-eval -p 6000"
             "  clj-socket-eval -p 6000 <<'EOF'"
             "  (def x 10)"
             "  (+ x 20)"
             "  EOF"
             "  clj-socket-eval -p 6000 --timeout 5000 \"(Thread/sleep 10000)\""]))

(defn error-msg [errors]
  (str "Error parsing command line:\n\n" (str/join \newline errors)))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options)
      (println (usage summary))

      errors
      (do
        (binding [*out* *err*]
          (println (error-msg errors))
          (println)
          (println (usage summary)))
        (exit! 1))

      :else
      (let [code (get-code arguments)]
        (cond
          (not code)
          (do
            (binding [*out* *err*]
              (println "Error: No code provided")
              (println "Provide code as an argument or via stdin (pipe/heredoc)")
              (println)
              (println (usage summary)))
            (exit! 1))

          (not (:port options))
          (do
            (binding [*out* *err*]
              (println "Error: --port is required"))
            (exit! 1))

          :else
          (eval-via-socket {:host (:host options)
                            :port (:port options)
                            :code code
                            :timeout-ms (:timeout options)}))))))
