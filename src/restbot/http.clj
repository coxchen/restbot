(ns restbot.http
  (:require [http.async.client :as c]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clj-time.core :as jt]
            [dire.core :refer [with-pre-hook! with-post-hook!]]))

;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;

(def CONN_TIMEOUT 10000)
(def GET_TIMEOUT 60000)
(defn- format-time [aTime] (.floatValue (with-precision 2 aTime)))
(defn- elapsed-since [timeStart] (format-time (/ (- (. System (nanoTime)) timeStart) 1000000.0) ))

(defn- json->map [jsonFilename]
  (with-open [rdr (clojure.java.io/reader jsonFilename)]
    (json/read rdr
               :key-fn (fn [k] (keyword k))
               :value-fn (fn [k v] v))))


;;;;;;;;;;;;;;;;;;;;

(def LOG_LEVEL (ref :INFO))

(defmacro with-log-level
  [logLevel & body]
  `(if (= @LOG_LEVEL ~logLevel)
    (do ~@body)))

(def DO_VALIDATION (ref false))

;;;;;;;;;;;;;;;;;;;;
;; VALIDATION
;;;;;;;;;;;;;;;;;;;;

(defn apply-validation
  [v-name v-fn toBeChecked]
  (let [errors (v-fn toBeChecked)]
    (do (if (> (count errors) 0)
          (do (println (str " - [" v-name "]") (count errors) "errors")
              (with-log-level :DEBUG (clojure.pprint/pprint errors)))
          (println (str " - [" v-name "]") "passed")))
    {v-name (not (> (count errors) 0))}))

(defn validate-json
  [toBeChecked & validations]
  (doall
   (for [[v-name v-fn] (partition 2 validations)]
     (apply-validation v-name v-fn toBeChecked))))

(defn validate-json-file
  [jsonFile & validations]
  (apply validate-json (json->map jsonFile) validations))

(with-pre-hook! #'validate-json-file
  (fn [jsonFile & validations]
    (println (str "# validating " jsonFile))))

;;;;;;;;;;;;;;;;;;;;

(defn- valid-json-file
  [jsonFilename]
  (try
    (merge {:size (.length (clojure.java.io/as-file jsonFilename))}
           {:content (json->map jsonFilename)})
    ;; server may return non-json message on AUTH FAILED
    (catch Exception e (do (println e) (println "# INVALID JSON")) nil)))

;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;

(defn- prn-resp
  [response]
  (with-log-level :DEBUG
    (println "   [KEYS] " (keys response)) ;; (:id :url :raw-url :status :headers :body :done :error)
    (println " [STATUS] " (c/status response))
    (println "[HEADERS] " (c/headers response))
    (println "   [DONE] " (c/done? response))
    (println "  [ERROR] " (c/error response))))

(defn- handle-http-error
  [response]
  (let [status (c/status response)
        error (c/error response)]
    (cond
     (and status (>= (status :code) 400)) {:error (select-keys status [:code :msg])}
     ((comp not nil?) error) {:error error})))

(defn- set-cookie?
  [response]
  (not-empty ((c/headers response) :set-cookie)))

(defn put!
  [api & opts]
  (with-open [client (c/create-client :connection-timeout CONN_TIMEOUT)]
    (let [response (c/PUT client (get api :url) :headers (get api :headers) :body (get api :body))]
      (c/await response)
      (prn-resp response)
      (if-let [error (handle-http-error response)]
        error
        {:cookies (if (set-cookie? response) (c/cookies response))
         :resp (c/string response)}))))

(def JSON_DIR (ref ""))

(defn- handle-http-ok
  [api response timestamps]
  (let [{:keys [nowStamp tStart waitTime]} timestamps
;;         jsonFilename (str @JSON_DIR (get api :name) "." tStart ".json")]
        jsonFilename (str @JSON_DIR (get api :name) ".json")]
    (with-open [wrt (io/writer jsonFilename)]
      (if (get api :stream?)
        (doseq [s (c/string response)]
          (.write wrt s))
        (do
          (c/await response)
          (if-let [serverResp (c/string response)] (.write wrt serverResp)))))
    (let [recvTime (format-time (- (elapsed-since tStart) waitTime))
          jsonResp (valid-json-file jsonFilename)]
      {:resp (if (and jsonResp (get api :resp))
               (assoc jsonResp :content ((get api :resp) (get jsonResp :content)))
               jsonResp)
       :cookies (if (set-cookie? response) (c/cookies response))
       :valid? (if (and @DO_VALIDATION jsonResp (get api :validations))
                 (apply validate-json (get jsonResp :content) (get api :validations)))
       :now nowStamp :waitTime waitTime :recvTime recvTime
       :latency (with-precision 5 :rounding FLOOR (* 1M (bigdec (+ waitTime recvTime))))})))

(defn get!
  [api & opts]
  (with-open [client (c/create-client :connection-timeout CONN_TIMEOUT :request-timeout GET_TIMEOUT)]
    (let [{:keys [cookies resp]} opts
          response (if (get api :stream?)
                     (c/stream-seq client :get (get api :url) :cookies cookies :timeout GET_TIMEOUT)
                     (c/GET client (get api :url) :cookies cookies :timeout GET_TIMEOUT))
          nowStamp (jt/now)
          tStart (. System (nanoTime))
          waitTime (elapsed-since tStart)
          timestamps {:nowStamp nowStamp :tStart tStart :waitTime waitTime}]
      (prn-resp response)
      (if-let [error (handle-http-error response)]
        (assoc error :now nowStamp)
        (handle-http-ok api response timestamps)))))

(defn post!
  [api & opts]
  (with-open [client (c/create-client :connection-timeout CONN_TIMEOUT :request-timeout GET_TIMEOUT)]
    (let [{:keys [cookies resp]} opts
          response (c/POST client (get api :url) :headers (get api :headers) :body (get api :body)
                           :cookies cookies :timeout GET_TIMEOUT)
          nowStamp (jt/now)
          tStart (. System (nanoTime))
          waitTime (elapsed-since tStart)
          timestamps {:nowStamp nowStamp :tStart tStart :waitTime waitTime}]
      (prn-resp response)
      (if-let [error (handle-http-error response)]
        (assoc error :now nowStamp)
        (handle-http-ok api response timestamps)))))

(defn del!
  [api & opts]
  (with-open [client (c/create-client :connection-timeout CONN_TIMEOUT :request-timeout GET_TIMEOUT)]
    (let [{:keys [cookies resp]} opts
          response (c/DELETE client (get api :url) :cookies cookies :timeout GET_TIMEOUT)
          nowStamp (jt/now)
          tStart (. System (nanoTime))
          waitTime (elapsed-since tStart)
          timestamps {:nowStamp nowStamp :tStart tStart :waitTime waitTime}]
      (prn-resp response)
      (if-let [error (handle-http-error response)]
        (assoc error :now nowStamp)
        (handle-http-ok api response timestamps)))))

(with-pre-hook! #'put!
  (fn [api & opts]
    (println "\n[put!]" (get api :url))))

(with-post-hook! #'put!
  (fn [result]
    (println "[put!]" result)))

(with-pre-hook! #'get!
  (fn [api & opts]
    (println "\n[get!]" (get api :url) "stream?" (get api :stream?) "api-keys: " (keys api))))

(with-post-hook! #'get!
  (fn [result]
    (println "[get!]" result)))

(with-pre-hook! #'post!
  (fn [api & opts]
    (println "\n[post!]" (get api :url) "with BODY" (get api :body))))

(with-post-hook! #'post!
  (fn [result]
    (println "[post!]" result)))

(with-pre-hook! #'del!
  (fn [api & opts]
    (println "\n[del!]" (get api :url))))

(with-post-hook! #'del!
  (fn [result]
    (println "[del!]" result)))
