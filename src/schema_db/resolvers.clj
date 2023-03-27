(ns schema-db.resolvers
  "This is temporarily part of rad-mapper. This plus schema-db.clj ought to be their own libaray"
  (:require
   [com.wsscode.pathom3.connect.indexes :as pci]
   [com.wsscode.pathom3.connect.operation :as pco]
   [com.wsscode.pathom3.interface.eql :as p.eql]
   [datahike.api        :as d]
   [datahike.pull-api   :as dp]
   [schema-db.db-util   :as du :refer [connect-atm]]
   [taoensso.timbre     :as log]))

;;;============================ Resolvers (communication with clients)  ==================================
;;; I think the key idea here for pathom-mediated composabiltiy is for each resolver to rename all db/id
;;; to context-specific names. These are currently #{:sdb/schema-id :sdb/elem-id :sdb/imported-schema-id}.
;;; (The last one isn't just a schema but a prefix too.)
;;; The simplest composition then is implemented as a flat table with values and foreign key references.
;;; I think, however, you can 'go deep' in the ::pco/output and still maintain. See all-schema-ids-r.
;;; See also person-resolver at Part 6, 43:26.

;;; Note that when you send an Ident, you get back a map with that ident and the response <=========
;;; (pathom-resolve {:schema/name "urn:oasis:names:specification:ubl:schema:xsd:Invoice-2"} [:sdb/schema-id])
;;; (pathom-resolve {:schema/name "urn:oagis-10.8.4:Nouns:Invoice"} [:sdb/schema-id])
;;; ==> {[:schema/name "urn:oasis:names:specification:ubl:schema:xsd:Invoice-2"] #:sdb{:schema-id 1230}}
(pco/defresolver schema-name->sdb-schema-id [_env {:schema/keys [name]}]
  {:sdb/schema-id (d/q `[:find ?e . :where [?e :schema/name ~name]] @(connect-atm))})

;;; (pathom-resolve {:schema/name "urn:oagis-10.8.4:Nouns:Invoice"} [:sdb/schema-object])
;;; [{"schema/name" : "urn:oagis-10.8.4:Nouns:Invoice"}, ["schema-object"]]
(pco/defresolver sdb-schema-id->sdb-schema-obj [env {:sdb/keys [schema-id]}]
  {:sdb/schema-object (du/resolve-db-id {:db/id schema-id} (connect-atm) #{:db/id})})

;;; (pathom-resolve [{:ccts/message-schema [:list/id  {:list/schemas [:sdb/schema-id :schema/name]}]}]) ; RIGHT!
;;; (pathom-resolve [{[:list/id :ccts/message-schema] {:list/schemas [:sdb/schema-id :schema/name]}}])  ; WRONG! WHY?
;;; (pathom-resolve {:list/id :ccts/message-schema} [:schema/name])
(pco/defresolver list-id->schema-list [env {:list/keys [id]}] ; e.g :list/id = :ccts/message-schema
  {::pco/output [{:list/id [:sdb/schema-id :schema/name]}]}
  (when (= id "ccts/message-schema")
    (when-let [schema-maps (->>
                            (d/q `[:find ?ent ?name ?topic
                                   :keys sdb/schema-id schema/name schema/topic
                                   :where
                                   [?ent :schema/name  ?name]
                                   [?ent :schema/topic ?topic]
                                   [?ent :schema/type ~id]]
                                 @(connect-atm))
                            (sort-by :schema/topic)
                            (mapv #(dissoc % :schema/topic))
                            not-empty)]
      {:list/id id
       :list/schemas schema-maps})))

(pco/defresolver message-schema [env input] ; THIS ONE WORKS (But...)
  {::pco/output [{:ccts/message-schema [:list/id {:list/schemas [:schema/name :sdb/schema-id]}]}]}
  {:ccts/message-schema {:list/id :ccts/message-schema}})

;;; (pathom-resolve [{[:sdb/schema-id 3569] [{:model/sequence [{:sdb/elem-id [:sp/name :sp/type :sp/minOccurs :sp/maxOccurs]}]}]}])
;;; (pathom-resolve [{[:sdb/schema-id 3569] [{:model/sequence [:sp/name :sp/type]}]}])
;;; (pathom-resolve [{[:sdb/schema-id 3569] [:schema/name]}])
;;; (pathom-resolve [{[:sdb/schema-id 3569] [:model/sequence]}])
(pco/defresolver sdb-schema-id->props [env {:sdb/keys [schema-id]}]
  {::pco/output [:schema/name :sdb/schema-id :schema/sdo :schema/type :schema/topic
                :schema/subversion :model/inlinedTypedef :schema/spec
                {:schema/importedSchema [:sdb/imported-schema-id]}
                {:model/sequence [:sdb/elem-id]}]}
  (-> (dp/pull @(connect-atm) '[*] schema-id) ; ToDo: could also do the :keys thing on the pull.
      (update :model/sequence (fn [s] (mapv #(-> % (assoc :sdb/elem-id (:db/id %)) (dissoc :db/id)) s)))
      (update :schema/importedSchema
              (fn [s]
                (mapv #(-> %
                           (assoc :sdb/imported-schema-id (:db/id %))
                           (dissoc :db/id)) s)))))

;;; (pathom-resolve [{[:sdb/elem-id 1280] [:schema/min-occurs]}])                          ; Simple
;;; (pathom-resolve [{[:schema/name invoice] [{[:sdb/elem-id 1280] [:schema/min-occurs]}]}]) ; YES!
;;; (pathom-resolve [{[:schema/name invoice] [{:model/sequence [:sp/name :sp/type :sp/minOccurs :sp/maxOccurs]}]}]) ; COMPLETE!
;;; (pathom-resolve [{[:sdb/schema-id 1230] [{:model/sequence [:sp/name :sp/type :sp/minOccurs :sp/maxOccurs]}]}]) ; COMPLETE!
(pco/defresolver elem-props [env {:sdb/keys [elem-id]}]
  {::pco/output [:doc/docString
                :sp/name
                :sp/type
                :sp/minOccurs
                :sp/maxOccurs]}
  (dp/pull @(connect-atm) '[*] elem-id))

;;; Nice thing about pathom (relative to GraphQL) is that you don't have to start at the root.
;;; This has nothing to do with ::pco/input; you can add this to a query anywhere.
(pco/defresolver current-system-time [_ _]
  {::pco/output [:server/time]}
  {:server/time (java.util.Date.)})

;;; ToDo: Of course, this needs to take an argument or be part of a user's project, etc.
;;; (pathom-resolve [{[:file/id :map-spec] [:user/data-file]}])
(pco/defresolver data-file [env {:file/keys [id]}]
  {::pco/output [:file/text]}
  (case id
    :source-data {:file/text (slurp "./data/messages/UBL-Invoice-2.1-Example.xml")}
    :target-data {:file/text " "}
    :map-spec    {:file/text (str   "// Map spec started " (java.util.Date.))}))

;;; ToDo: Looks suspicious!
;;; Try it with (currently!):
;;;           [:sdb/elem-id 1305] (UBL "Invoice" element)
;;;           [:sdb/elem-id 5230] (OAGIS "Invoice" element)
#_(defn substructure-resolver-r
  "Return substructure for the argument elem-id"
  [elem-id]
  (if-let [owning-schema (d/q `[:find ?s :where [?s :model/sequence ~elem-id]] @(connect-atm))]
    (let [ref-1 (d/q `[:find [?rs ...] :where [~owning-schema :schema/importedSchema ?rs]] @(connect-atm))
          ref-2 (d/q `[:find [?rs ...] :where [~owning-schema :model/inlinedTypedef  ?rs]] @(connect-atm))
          _refs (into ref-1 ref-2)
          _elem-info (dp/pull @(connect-atm) '[*] elem-id)]
      (if-let [_ref-schema nil #_(schema-containing-ref)]
        :found-it
        (log/warn "Could not find referenced schema for" elem-id)))
    (log/warn "Could not find owning schema for" elem-id)))

(def indexes
  (pci/register [schema-name->sdb-schema-id
                 sdb-schema-id->sdb-schema-obj
                 sdb-schema-id->props
                 elem-props
                 list-id->schema-list
                 message-schema
                 data-file
                 current-system-time]))

;;; (pathom-resolve {:list/id :ccts/message-schema} [:schema/name])
(defn pathom-resolve
  "Uses the indexes to respond to a query.
   ident-map: a single-entry map, the key of which names an identity condition,
              such as a db/ident, the value of which is a value for the identity condition.
   outputs: a vector of properties (the :pco/outputs of resolvers) that are sought."
  [ident-map outputs]
  (log/info "Pathom resolve: db = " @(connect-atm) " ident-map = " ident-map " outputs= " outputs)
  (p.eql/process indexes ident-map outputs))
