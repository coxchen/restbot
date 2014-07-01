(ns restbot.core
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [clj-schema.validation :refer [validation-errors]]
            [restbot.http :as http]
            [plumbing.graph :as graph])
  (:use [plumbing.core]))

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
  (let [{:keys [headers body validations stream?]} apiMore
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
              :stream? ~stream?
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

(defn- make-task-graph
  [specs serverSym authSym]
  (reduce merge
          (for [[specKey specParam specBody] (partition 3 specs)]
            {specKey (cons 'fnk
                           (list (conj specParam serverSym authSym)
                                 (list 'do! (list specBody
                                                  :server-url (list :url serverSym))
                                       :cookies (if-not (nil? authSym) (list :cookies authSym)))))})))

(defmacro def-task
  [graphName & specs]
  (let [server# (gensym 'server)
        auth# (gensym 'auth)
        no-op# (fn [])
        graphKey (keyword graphName)
        tasks (make-task-graph specs server# auth#)
        taskGraph (-> tasks
                      (assoc (keyword auth#) `(fnk [] (~no-op#)))
                      (with-meta {:auth-step (keyword auth#)
                                  :server-key (keyword server#)}))]
    `(do
       (def ~graphName
         ~taskGraph)
       (swap! tasks assoc (keyword '~graphName) ~graphName))))

;;;;;;;;;;;;;;;;;;;;
;; RUN!!!
;;;;;;;;;;;;;;;;;;;;

(defn do!
  [req & opts]
  (let [req (if (fn? req) (req) req)]
    (condp = (req :method)
      :GET  (apply http/get! req opts)
      :PUT  (apply http/put! req opts)
      :POST (apply http/post! req opts)
      :DEL  (apply http/del! req opts)
      (str "UNSUPPORTED REQUEST TYPE" (req :type)))))

(defn- extract-auth-step
  [server]
  (condp = (get-in server [:auth :type])
    :cookies (get-in server [:auth :req])
    nil))

(defn run!
  [server taskGraph]
  (let [serverUrl (server :url)
        authStepKey ((meta taskGraph) :auth-step)
        serverKey ((meta taskGraph) :server-key)
        execGraph (if-let [auth-step (extract-auth-step server)]
                    (assoc taskGraph authStepKey (fnk [] (do! (auth-step :server-url serverUrl))))
                    taskGraph)]
    (into {} ((graph/compile execGraph) {serverKey server}))))

;;;;;;;;;;;;;;;;;;;;
;; OTHERs
;;;;;;;;;;;;;;;;;;;;

(defn current-log-level [] @http/LOG_LEVEL)

(defn log-level [level] (dosync (ref-set http/LOG_LEVEL level)))

(defn toggle-validation [] (dosync (ref-set http/DO_VALIDATION true)))

(defn set-json-dir [jsonDir] (dosync (ref-set http/JSON_DIR jsonDir)))