(ns schema-db.resolvers-test
  "Testing schema-db resolvers."
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.wsscode.pathom3.interface.smart-map :as psm]
   [datahike.api             :as d]
   [datahike.pull-api        :as dp]
   [schema-db.resolvers      :as res :refer [indexes]]
   [schema-db.db-util        :as du  :refer [connect-atm]]))

(def last-step (atom nil))

;;; (try-sm {:schema/name "urn:oagis-10.8:Nouns:Invoice"} :sdb/schema-object) ; NOPE!
;;; (try-sm {:schema/name "urn:oagis-10.8:Nouns:Invoice"} :sdb/schema-id)
(deftest resolver-calls
  (testing "Testing calls to individual resolvers and multi-steps of resolver calls."
    (testing "Testing one steps."
      (is (number? (->> {:schema/name "urn:oagis-10.8.4:Nouns:Invoice"}
                        res/schema-name->sdb-schema-id
                        :sdb/schema-id
                        (reset! last-step))))
      (is (= :oagi (->> {:sdb/schema-id @last-step}
                        res/sdb-schema-id->sdb-schema-obj
                        :sdb/schema-object
                        :schema/sdo))))))

;;; This doesn't work like https://pathom3.wsscode.com/docs/resolvers. Perhaps investigate "resolver metadata":
;;; /Now check a few examples of how we can accomplish the same using the a smart map leveraged by the resolvers meta-data:/
;;; (->> {:schema/name "urn:oagis-10.8.4:Nouns:Invoice"}
;;;      (psm/smart-map indexes))









(defn try-sm
  "Return as specified by the a"
  [ident-map prop]
  (connect-atm)
  (-> ident-map
      (psm/smart-map indexes)
      prop))
