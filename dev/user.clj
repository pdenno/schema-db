(ns user
  (:require
   [clojure.tools.namespace.repl :as tools-ns :refer [set-refresh-dirs]]
   [mount.core :as mount]
   ;; this is the top-level dependent component...mount will find the rest via ns requires
   ;; ToDo: Consider using integrant, since that what kit-clj / exerciser use.
   [schema-db.core]
   [schema-db.db-util   :as du]
   [taoensso.timbre     :as log]))

;;; https://clojure.org/guides/dev_startup_time
;;; (binding [*compile-files* true] (require 'user :reload-all)) ==> No such file or directory.
;;; So far, I have to compile this in CIDER.
(set-refresh-dirs "src/schema-db" "test/schema-db")

(defn start
  "Start the server"
  []
  (mount/start)
  (if (du/connect-atm)
    (log/info "Starting schema-db: db connection = " @(du/connect-atm))
    (log/error "connection atom is nil.")))

(defn go [] (start))

(defn stop
  "Stop the server"
  [] (mount/stop))

(defn unstuck []
  (tools-ns/refresh))

(defn restart
  "Stop, reload code, and restart the server. If there is a compile error,
   use: unstuck to recompile, and then use `start` once things are good."
  []
  (stop)
  (tools-ns/refresh :after 'user/start))
