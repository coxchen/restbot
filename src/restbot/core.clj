(ns restbot.core
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [clj-schema.validation :refer [validation-errors]]
            [restbot.http :as http]))

;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *error-mode* :exit)
(def ^{:dynamic true :private true} *global-options* (atom {}))

(defn error [msg]
  (if (= :exit (or (:error-mode @*global-options*) *error-mode*))
    (do (doto System/err (.println msg)) (System/exit 1))
    (throw (RuntimeException. msg))))

(defonce apis (atom (hash-map)))
(defonce servers (atom (hash-map)))
(defonce tasks (atom (hash-map)))

;;;;;;;;;;;;;;;;;;;;
;; SERVER
;;;;;;;;;;;;;;;;;;;;

(defmacro def-server
  [serverName serverUrl & opts]
  (let [{:keys [auth]} opts
        serverKeyword (keyword serverName)]
    `(do
       (def ~serverName
         {:name (str '~serverName)
          :url ~serverUrl
          :auth ~auth})
       (swap! servers assoc ~serverKeyword ~serverName))))


;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;

(defn- replace-with-param
  [basestr param]
  (string/replace basestr
                  (re-pattern (str "\\{" (name (first param)) "\\}"))
                  (str (second param))))

(defn apply-api-params
  [apiUrl paramMap]
  (reduce replace-with-param (into [apiUrl] paramMap)))

(defmacro def-api
  [apiName apiMethod apiUrl & apiMore]
  (let [{:keys [headers body validations]} apiMore
        apiKeyword (keyword apiName)]
    `(do
       (defn ~apiName [& apiOpts#]
         (fn [& apiRuntime#]
           (let [{:keys [~'body ~'headers ~'params ~'resp]} apiOpts#
                 {:keys [~'server-url]} apiRuntime#]
             {:name (str '~apiName)
              :method ~apiMethod
              :url (str ~'server-url
                        (if ~'params (apply-api-params ~apiUrl ~'params) ~apiUrl))
              :headers (if ~'headers ~'headers ~headers)
              :body (if ~'body ~'body ~body)
              :resp ~'resp
              :validations ~validations})))
       (swap! apis assoc ~apiKeyword {:name (str '~apiName)
                                      :method ~apiMethod
                                      :url ~apiUrl}))))

;;;;;;;;;;;;;;;;;;;;
;; SCHEMA
;;;;;;;;;;;;;;;;;;;;

(def apply-schema validation-errors)
(defn comply? [schema] (partial apply-schema schema))

;;;;;;;;;;;;;;;;;;;;
;; TASK
;;;;;;;;;;;;;;;;;;;;

(defmacro def-task
  [taskName & taskBody]
  `(do
     (def ~taskName
       {:name (str '~taskName) :tasks (conj [] ~@taskBody)})
     (swap! tasks assoc (keyword '~taskName) ~taskName)))

;;;;;;;;;;;;;;;;;;;;
;; RUN!!!
;;;;;;;;;;;;;;;;;;;;

(defn do!
  [req & opts]
  (let [req (if (fn? req) (req) req)]
    (condp = (req :method)
      :GET (apply http/get! req opts)
      :PUT (apply http/put! req opts)
      (str "UNSUPPORTED REQUEST TYPE" (req :type)))))

(defn run!
  [server & tasks]
  (let [server-url (server :url)
        do-fn (condp = (get-in server [:auth :type])
                :cookies (let [auth-req (get-in server [:auth :req])
                               {:keys [cookies error]} (do! (auth-req :server-url server-url))]
                           (if error
                             nil
                             #(do! (% :server-url server-url) :cookies cookies)))
                #(do! (% :server-url server-url)))]
    (if do-fn (doall (map do-fn (mapcat :tasks tasks))))))

;;;;;;;;;;;;;;;;;;;;
;; LOG
;;;;;;;;;;;;;;;;;;;;

(defn current-log-level [] @http/LOG_LEVEL)

(defn log-level [level] (dosync (ref-set http/LOG_LEVEL level)))
