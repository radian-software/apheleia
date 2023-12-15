(ns hello
  (:require [java-time.api :as t]))

(defn time-str
  "Returns a string representation of a datetime in the local time zone."
  [instant]
  (t/format
   (t/with-zone (t/formatter "hh:mm a") (t/zone-id))
   instant))

(defn run [opts]
  (println "Hello world, the time is" (time-str (t/instant))))
