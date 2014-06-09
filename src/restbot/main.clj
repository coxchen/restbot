(ns restbot.main
  (:use [restbot.core])
  (:use [clojure.string :only [join]]
        [clojure.tools.cli :only [cli]]
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

(defn run
  "Run tasks on a specific server"
  [ & args]
  (do
    (load-restbot-file)
    (if-let [server (get @servers (keyword (first args)))]
      (do
        (when-let [targetTask (get @tasks (keyword (second args)))]
          (let [taskKeys (keys (dissoc targetTask (:auth-step (meta targetTask))))]
            (println "RUN on" (:name server) "with" (clojure.string/join taskKeys)))
          (run! server targetTask)
          )
        )
    )))

(defn version
  "Print version for RestBot and the current JVM."
  []
  (println "RestBot" (System/getProperty "RESTBOT_VER")
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

(defn print-help []
  (println "Usage:restbot [-f restbot.clj] command args")
  (println "Commands available:")
;;   (println "init                           Initialize restbot, create a sample restbot file in current folder")
  (println "run <server> <task> <args>    Run user-defined restbot tasks against certain server")
  (println "show <server>                 Show server info")
  (println "list server/task/api          List server/task/api defined in restbot.clj")
  (println "upgrade                       Upgrade restbot to a latest version."))

(defn -main [ & args]
  (let [[options extra-args]
        (cli args ["-f" "--file" "Which restbot file to be executed."])
        {:keys [file]} options
        cmd (first extra-args)
        args (next extra-args)]
    (when file
      (reset! special-restbot-file file))
    (case cmd
;;       "init" (apply init args)
      "run" (apply run args)
      "list" (apply list-things args)
      "show" (apply show args)
      "version" (apply version args)
      (print-help))))
