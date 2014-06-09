(ns restbot.utils
  (:require [clojure.data.json :as json]
            [clj-time.core :as jt]
            [clj-time.coerce :as coerce]
            [clj-time.format :refer [formatter parse unparse]]))

(defn quarter
  ([] (quarter (jt/now) 0))
  ([interval] (quarter (jt/now) interval))
  ([someTime interval]
   (let [qInHr (* (int (/ (jt/minute someTime) 15)) 15) ;; (instance? org.joda.time.DateTime someTime) => true
         qThis (-> someTime (.withSecondOfMinute 0) (.withMillisOfSecond 0) (.withMinuteOfHour qInHr))]
     (jt/plus qThis (jt/minutes (* interval 15))))))

(def evtTimeFormat (formatter "yyyy-MM-dd+HH:mm:ss.SSS+z"))
(def parse-time (partial parse evtTimeFormat))
(def unparse-time (partial unparse evtTimeFormat))

(def from-long coerce/from-long)
(def to-long coerce/to-long)

(defmulti joda-time class)
(defmethod joda-time org.joda.time.DateTime [someTime] someTime)
(defmethod joda-time String [someTime] (parse-time someTime))
(defmethod joda-time Long [someTime] (from-long someTime))

(def map2json json/json-str)
