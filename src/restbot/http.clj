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
;; VALIDATION
;;;;;;;;;;;;;;;;;;;;

(defn apply-validation
  [v-name v-fn toBeChecked]
  (let [errors (v-fn toBeChecked)]
    (do (if (> (count errors) 0)
          (do
            (println (str "[" v-name "]") (count errors) "errors")
            (clojure.pprint/pprint errors)
            )
          (println (str "[" v-name "]") "passed")))
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

(def LOG_LEVEL (ref :INFO))

(defn- prn-resp
  [response]
  (if (= @LOG_LEVEL :DEBUG)
    (do
      (println "   [KEYS] " (keys response)) ;; (:id :url :raw-url :status :headers :body :done :error)
      (println " [STATUS] " (c/status response))
      (println "[HEADERS] " (c/headers response))
      (println "   [DONE] " (c/done? response))
      (println "  [ERROR] " (c/error response)))))

(defn- handle-http-error
  [response]
  (let [status (c/status response)
        error (c/error response)]
    (cond
     (and status (>= (status :code) 400)) {:error (select-keys status [:code :msg])}
     ((comp not nil?) error) {:error error})))

(defn put!
  [api & opts]
  (with-open [client (c/create-client :connection-timeout CONN_TIMEOUT)]
    (let [response (c/PUT client (api :url) :headers (api :headers) :body (api :body))]
      (c/await response)
      (prn-resp response)
      (if-let [error (handle-http-error response)]
        error
        {:cookies (if ((c/headers response) :set-cookie) (c/cookies response))
         :resp (c/string response)}))))

(defn- handle-get-ok
  [api response timestamps]
  (let [{:keys [nowStamp tStart waitTime]} timestamps
        jsonFilename (str (api :name) ".json")]
    (with-open [wrt (io/writer jsonFilename)]
      (doseq [s (c/string response)]
        (.write wrt s)))
    (let [recvTime (format-time (- (elapsed-since tStart) waitTime))
          jsonResp (valid-json-file jsonFilename)]
      {:resp (if (and jsonResp (api :resp))
               (assoc jsonResp :content ((api :resp) (get jsonResp :content)))
               jsonResp)
       :valid? (if (and jsonResp (api :validations))
                 (apply validate-json (get jsonResp :content) (api :validations)))
       :now nowStamp :waitTime waitTime :recvTime recvTime
       :latency (with-precision 5 :rounding FLOOR (* 1M (bigdec (+ waitTime recvTime))))})))

(defn get!
  [api & opts]
  (with-open [client (c/create-client :connection-timeout CONN_TIMEOUT :request-timeout GET_TIMEOUT)]
    (let [{:keys [cookies resp]} opts
          response (c/stream-seq client :get (api :url) :cookies cookies :timeout GET_TIMEOUT)
          nowStamp (jt/now)
          tStart (. System (nanoTime))
          waitTime (elapsed-since tStart)
          timestamps {:nowStamp nowStamp :tStart tStart :waitTime waitTime}]
      (prn-resp response)
      (if-let [error (handle-http-error response)]
        (assoc error :now nowStamp)
        (handle-get-ok api response timestamps)))))

(with-pre-hook! #'put!
  (fn [api & opts]
    (println "[put!]" (api :url))))

(with-post-hook! #'put!
  (fn [result]
    (println "[put!]" result)))

(with-pre-hook! #'get!
  (fn [api & opts]
    (println "[get!]" (api :url))))

(with-post-hook! #'get!
  (fn [result]
    (println "[get!]" result)))

