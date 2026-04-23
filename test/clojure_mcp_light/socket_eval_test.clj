(ns clojure-mcp-light.socket-eval-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [clojure-mcp-light.socket-eval :as se]))

;; ============================================================================
;; CLI parsing
;; ============================================================================

(deftest cli-parsing-test
  (testing "host defaults to localhost"
    (let [{:keys [options]} (cli/parse-opts
                             ["-p" "6000"] se/cli-options)]
      (is (= "localhost" (:host options)))))

  (testing "timeout defaults to 120000ms"
    (let [{:keys [options]} (cli/parse-opts
                             ["-p" "6000"] se/cli-options)]
      (is (= 120000 (:timeout options)))))

  (testing "port is parsed as long"
    (let [{:keys [options]} (cli/parse-opts
                             ["-p" "6000"] se/cli-options)]
      (is (= 6000 (:port options)))))

  (testing "invalid port fails validation"
    (let [{:keys [errors]} (cli/parse-opts
                            ["-p" "-1"] se/cli-options)]
      (is (seq errors)))))

;; ============================================================================
;; get-code precedence
;; ============================================================================

(deftest get-code-arg-beats-stdin-test
  (testing "argument takes precedence over stdin"
    (with-redefs [se/has-stdin-data? (constantly true)]
      (with-in-str "(from-stdin)"
        (is (= "(from-arg)" (se/get-code ["(from-arg)"])))))))

(deftest get-code-stdin-fallback-test
  (testing "stdin used when no args"
    (with-redefs [se/has-stdin-data? (constantly true)]
      (with-in-str "(from-stdin)"
        (is (= "(from-stdin)" (se/get-code [])))))))

(deftest get-code-nil-when-neither-test
  (testing "nil when no args and no stdin"
    (with-redefs [se/has-stdin-data? (constantly false)]
      (is (nil? (se/get-code []))))))

;; ============================================================================
;; End-to-end socket transport (against a real ServerSocket)
;; ============================================================================

(defn start-echo-server
  "Start a minimal TCP server that echoes received bytes back to the client.
   Returns {:server ServerSocket :port int :received atom}.
   The :received atom accumulates the bytes the client sent, so tests can
   verify what eval-via-socket actually wrote on the wire."
  []
  (let [server (java.net.ServerSocket. 0)
        port (.getLocalPort server)
        received (atom "")
        accept-loop (future
                      (try
                        (with-open [client (.accept server)]
                          (let [in (.getInputStream client)
                                out (.getOutputStream client)
                                baos (java.io.ByteArrayOutputStream.)
                                buf (byte-array 4096)]
                            (loop []
                              (let [n (.read in buf)]
                                (when (pos? n)
                                  (.write baos buf 0 n)
                                  (recur))))
                            (let [bytes (.toByteArray baos)]
                              (reset! received (String. bytes "UTF-8"))
                              (.write out bytes)
                              (.flush out))))
                        (catch Exception _ nil)))]
    {:server server :port port :received received :loop accept-loop}))

(deftest eval-via-socket-roundtrip-test
  (testing "writes code to socket and prints response to stdout"
    (let [{:keys [server port received loop]} (start-echo-server)]
      (try
        (let [out-str (with-out-str
                        (se/eval-via-socket {:host "localhost"
                                             :port port
                                             :code "(+ 1 2)"
                                             :timeout-ms 5000}))]
          @loop
          (is (= "(+ 1 2)" @received))
          (is (= "(+ 1 2)" out-str)))
        (finally
          (.close server))))))

(deftest eval-via-socket-repairs-delimiters-test
  (testing "delimiter errors are repaired before sending"
    (let [{:keys [server port received loop]} (start-echo-server)]
      (try
        (with-out-str
          (se/eval-via-socket {:host "localhost"
                               :port port
                               :code "(+ 1 2"
                               :timeout-ms 5000}))
        @loop
        (is (= "(+ 1 2)" @received)
            "Unbalanced input should be repaired to balanced form")
        (finally
          (.close server))))))

(deftest eval-via-socket-passes-through-valid-code-test
  (testing "already-balanced code is sent unchanged"
    (let [{:keys [server port received loop]} (start-echo-server)]
      (try
        (with-out-str
          (se/eval-via-socket {:host "localhost"
                               :port port
                               :code "(def x 10)\n(+ x 20)"
                               :timeout-ms 5000}))
        @loop
        (is (= "(def x 10)\n(+ x 20)" @received))
        (finally
          (.close server))))))

;; ============================================================================
;; Connection failure path
;; ============================================================================

(deftest connection-refused-exits-1-test
  (testing "connection refused prints to stderr and exits 1"
    (let [exit-code (atom nil)
          err-out (java.io.StringWriter.)]
      (with-redefs [se/exit! (fn [code]
                               (reset! exit-code code)
                               (throw (ex-info "exit" {:code code})))]
        (binding [*err* err-out]
          (try
            ;; Port 1 is virtually certain to be closed
            (se/eval-via-socket {:host "localhost"
                                 :port 1
                                 :code "(+ 1 2)"
                                 :timeout-ms 5000})
            (catch clojure.lang.ExceptionInfo _ nil))))
      (is (= 1 @exit-code))
      (is (str/includes? (str err-out) "Could not connect")))))
