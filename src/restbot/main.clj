(ns restbot.main
  (:use [restbot.core])
  (:use [clojure.string :only [join]]
        [clojure.tools.cli :only [parse-opts]]
        [clojure.java.io :only [file reader writer]])
  (:gen-class))

(def ^:private special-restbot-file (atom nil))

(def ^{:private true :tag String} RESTBOT-DIR "restbot.original.pwd")

(defn- get-restbot-dir []
  (System/getProperty RESTBOT-DIR "."))

(defn- create-restbot-ns []
  (create-ns (gensym "user-restbot")))

(defn- load-restbot-file []
  (try
    (binding [*ns* (create-restbot-ns)]
      (refer-clojure)
      (use '[restbot core utils])
      (use '[plumbing.core])
      (require '[plumbing.graph :as graph])
      (load-file
       (or @special-restbot-file (str (get-restbot-dir) "/restbot.clj"))))
    (catch java.io.FileNotFoundException e
      (error (str "restbot file" (str (get-restbot-dir) "/restbot.clj") "not found.")))))

(defn- init-json-dir []
  (if-not (.isDirectory (java.io.File. "json"))
    (.mkdir (java.io.File. "json")))
  (set-json-dir "json/"))

(defn run-restbot
  [args]
  (if-let [server (get @servers (keyword (first args)))]
    (do
      (when-let [targetTask (get @tasks (keyword (second args)))]
        (let [taskKeys (keys (dissoc targetTask (:auth-step (meta targetTask))))]
          (println "RUN on" (:name server) "with" (clojure.string/join taskKeys)))
        (run! server targetTask)))))

(defn run
  "Run tasks on a specific server"
  [ & args]
  (do
    (load-restbot-file)
    (init-json-dir)
    (run-restbot args)))

(defn version
  "Print version for RestBot and the current JVM."
  []
  (println "RestBot" (System/getenv "RESTBOT_VERSION")
           "on Java" (System/getProperty "java.version")
           (System/getProperty "java.vm.name")))

(defn- show-things
  [things show-fn]
  (do
    (doseq [k (keys things)]
      (let [t (k things)]
        (println (show-fn t))
        ))))

(defn list-things
  "List something"
  [ & args]
  (do
    (load-restbot-file)
    (if-let [category (first args)]
      (condp = (keyword category)
        :server (show-things @servers #(str "- [" (get % :name) "] " (get % :url)))
        :task (show-things
               @tasks
               #(str "# " (get % :name) "\n"
                     (clojure.string/join "\n"
                      (map (fn [t] (format "  - %s" t))
                           (into [] (for [task (get % :tasks)]
                                      (let [t (apply task nil)]
                                        (str "[" (name (:method t)) "]["
                                             (:name t nil) "] " (:url t)))))))))
        :api (show-things @apis #(str "- [" (name (get % :method)) "][" (get % :name) "]\n  "
                                      (get % :url)))))))

(defn show
  "Show server info"
  [ & args]
  (do
    (load-restbot-file)
    (if-let [server-name (first args)]
      (let [server ((keyword server-name) @servers)
            url (:url server)]
        (println "SERVER: " server)
        (println "   URL: " url)))))

(defn print-help [summary]
  (println "Usage:restbot [-f restbot.clj] [-d] [-v] command args\n")
  (println summary)
  (println "----------")
  (println "Commands available:")
;;   (println "init                           Initialize restbot, create a sample restbot file in current folder")
  (println "run <server> <task>           Run user-defined restbot tasks against certain server")
  (println "show <server>                 Show server info")
  (println "list <server/task/api>        List server/task/api defined in restbot.clj")
  (println "upgrade                       Upgrade restbot to a latest version."))

(def cli-opts
  [["-f" "--file" "Which restbot file to be executed."]
   ["-d" nil "Output debug message"
    :id :debug :default false
    :assoc-fn (fn [m k _] (update-in m [k] not))]
   ["-v" nil "Perform validation on JSON response"
    :id :validation :default false
    :assoc-fn (fn [m k _] (update-in m [k] not))]
   ])

(defn -main [ & args]
  (let [{:keys [options arguments summary]} (parse-opts args cli-opts)
        {:keys [file debug validation]} options
        cmd (first arguments)
        args (next arguments)]
    (when file (reset! special-restbot-file file))
    (when debug (log-level :DEBUG))
    (when validation (toggle-validation))
    (case cmd
;;       "init" (apply init args)
      "run" (apply run args)
      "list" (apply list-things args)
      "show" (apply show args)
      "version" (apply version args)
      (print-help summary))))
