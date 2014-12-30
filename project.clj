(defproject restbot "0.4.0"
  :description "A testing framework for RESTful/JSON API"
  :url "https://github.com/coxchen/restbot"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :author "cox chen(coxchen@gmail.com)"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.slf4j/slf4j-nop "1.6.4"]
                 [org.clojure/tools.cli "0.3.1"]
                 [http.async.client "0.5.2"]
                 [org.clojure/data.json "0.2.2"]
                 [org.clojars.runa/clj-schema "0.9.4"]
                 [dire "0.4.4"]
                 [clj-time "0.6.0"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [prismatic/plumbing "0.3.1"]]
  :plugins [[lein-gorilla "0.2.0"]]
  :main restbot.main)
