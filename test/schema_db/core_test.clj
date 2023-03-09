(ns schema-db.core-test
  "Functions to read XML to structures that the DB can use."
  (:require
   [clojure.test :refer [deftest is testing]]
   [datahike.api             :as d]
   [datahike.pull-api        :as dp]
   [schema-db.core           :as core :refer [connect-db]]
   [schema-db.generic-schema :as gs]
   [schema-db.resolvers      :as res :refer [pathom-resolve]]))

(defn schema-for-sdo
  "Return a list of schema for the argument SDO."
  [sdo]
  (d/q '[:find [?name ...]
         :in $ ?sdo
         :where [?e :schema/sdo ?sdo]
                [?e :schema/name ?name]]
       @(connect-db) sdo))

(deftest counting-schema
  (testing "Testing that the number of schema for each SDO/version are as expected."
    (testing "Testing schema count for OAGI 10.8.4 is 137."  (is (== 137 (-> :oagi  schema-for-sdo count))))
    (testing "Testing schema count for OASis 2.3 is 98."     (is (==  98 (-> :oasis schema-for-sdo count))))
    (testing "Testing schema count for QIF 3.0 is 22."       (is (==  22 (-> :qif   schema-for-sdo count))))))

(deftest files-look-good
  (testing "Testing that files look okay." ; ToDo: I haven't done much comparing to the XSD.
    (testing "Testing an OAGIS file."
      (is (= (-> "test/examples/OAGISinvoice.edn" slurp read-string)
             (gs/read-schema-file (str core/oagis-10-8-root "Nouns/Invoice.xsd")))))

    (testing "Testing an UBL file."
      (is (= (-> "test/examples/UBLinvoice.edn" slurp read-string)
             (gs/read-schema-file (str core/ubl-root "maindoc/UBL-Invoice-2.3.xsd")))))

    (testing "Testing a QIF file."
      (is (= (-> "test/examples/QIFResults.edn" slurp read-string)
             (gs/read-schema-file (str core/qif-root "QIFApplications/QIFResults.xsd")))))))
