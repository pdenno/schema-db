(ns dev.util
  (:require
   [clojure.set]
   [datahike.api      :as d]
   [schema-db.db-util :refer [connect-atm]]
   [schema-db.schema  :refer [db-schema+]]))

(defn find-root
  "Walking through the object, collect
    - map keys in the 'ROOT' namespace, and
    - :xml/tag's that have a value that is ROOT."
  [obj & {:keys [ns-name] :or {ns-name "ROOT"}}]
  (let [res-atm (atom #{})]
    (letfn [(fr [obj]
              (cond (map? obj) (doseq [[k v] (seq obj)]
                                 (when (= ns-name (namespace k))
                                   (swap! res-atm conj k))
                                 (when (and (= k :xml/tag) (= ns-name (namespace v)))
                                   (swap! res-atm conj [:xml/tag v]))
                                 (fr v))
                    (vector? obj) (doall (map fr obj))))]
      (fr obj))
    @res-atm))

;;;================================ DB analytics ==============================
(defn unused-db-idents []
  (-> (clojure.set/difference
       (-> db-schema+ keys set)
       (-> (d/q '[:find [?typ ...]
                  :where [_ ?typ _]]
                @(connect-atm))
           set))
      sort vec))

(defn ccts-used
  "Return a sorted vector of the CCTS used."
  [obj]
  (let [used (atom #{})]
    (letfn [(cu [obj]
              (cond (map? obj)       (doseq [[k v] (seq obj)]
                                       (when (= "cct" (namespace k))
                                         (swap! used conj k))
                                       (cu v))
                    (vector? obj)    (doall (map cu obj))))]
      (cu obj)
      (-> used deref sort vec))))
