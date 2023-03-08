(ns schema-db.util
  (:require
   [taoensso.timbre              :as log]))

;;; ToDo: The problem with output to log/debug might have to do with *err* not defined in cljs.
(defn custom-output-fn
  " - I don't want :hostname_ and :timestamp_ in the log output preface text..
    - I don't want any preface text in schema-db.parse output."
  ([data] (custom-output-fn nil data))
  ([opts data]
   (if (=  (:?ns-str data) "schema-db.generic-schema") ; ToDo: Needs investigation.
     (apply str (:vargs data)) ; So it can do simple indented call tracing.
     (taoensso.timbre/default-output-fn opts (dissoc data :hostname_ :timestamp_)))))

(defn config-log
  "Configure Timbre: set reporting levels and specify a custom :output-fn."
  [min-level]
  (if (#{:trace :debug :info :warn :error :fatal :report} min-level)
    (log/set-config!
     (-> log/*config*
         (assoc :output-fn #'custom-output-fn)
         (assoc :min-level [[#{"datahike.*"} :error]
                            [#{"datascript.*"} :error]
                            [#{"*"} min-level]])))
    (log/error "Invalid timbre reporting level:" min-level)))
