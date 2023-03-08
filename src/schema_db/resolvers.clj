(ns schema-db.resolvers
  "This is temporarily part of rad-mapper. This plus schema-db.clj ought to be their own libaray"
  (:require
   [com.wsscode.pathom.core      :as p]
   [com.wsscode.pathom.connect   :as pc]
   ;[com.wsscode.pathom3.connect.indexes :as pci] ; See  https://pathom3.wsscode.com/docs/
   ;[com.wsscode.pathom3.connect.operation :as pco]
   ;[com.wsscode.pathom3.interface.eql :as p.eql]
   [datahike.api        :as d]
   [datahike.pull-api   :as dp]
   [schema-db.db-util :as du]))

;;;============================ Resolvers (communication with clients)  ==================================
;;; I think the key idea here for pathom-mediated composabiltiy is for each resolver to rename all db/id
;;; to context-specific names. These are currently #{:sdb/schema-id :sdb/elem-id :sdb/imported-schema-id}.
;;; (The last one isn't just a schema but a prefix too.)
;;; The simplest composition then is implemented as a flat table with values and foreign key references.
;;; I think, however, you can 'go deep' in the ::pc/output and still maintain. See all-schema-ids-r.
;;; See also person-resolver at Part 6, 43:26.

;;; Note that when you send an Ident, you get back a map with that ident and the response <=========
;;; (pathom-resolve [{[:schema/name "urn:oasis:names:specification:ubl:schema:xsd:Invoice-2"] [:sdb/schema-id]}])
;;; (pathom-resolve [{[:schema/name "urn:oagis-10.8:Nouns:Invoice"] [:sdb/schema-id]}])
;;; ==> {[:schema/name "urn:oasis:names:specification:ubl:schema:xsd:Invoice-2"] #:sdb{:schema-id 1230}}
(pc/defresolver schema-by-name-r [env {:schema/keys [name]}]
  {::pc/input #{:schema/name}
   ::pc/output [:sdb/schema-id]}
  {:sdb/schema-id (d/q `[:find ?e . :where [?e :schema/name ~name]] @du/conn)})

;;; (pathom-resolve [{[:schema/name "urn:oagis-10.8:Nouns:Invoice"] [:sdb/schema-object]}])
;;; [{"schema/name" : "urn:oagis-10.8:Nouns:Invoice"}, ["schema-object"]]
(pc/defresolver full-schema-r [env {:sdb/keys [schema-id]}]
  {::pc/input #{:sdb/schema-id}
   ::pc/output [:sdb/schema-object]}
   {:sdb/schema-object (du/resolve-db-id {:db/id schema-id} du/conn #{:db/id})})

;;; (pathom-resolve [{:ccts/message-schema [:list/id  {:list/schemas [:sdb/schema-id :schema/name]}]}]) ; RIGHT!
;;; (pathom-resolve [{[:list/id :ccts/message-schema] {:list/schemas [:sdb/schema-id :schema/name]}}])  ; WRONG! WHY?
(pc/defresolver list-r [env {:list/keys [id]}] ; e.g :list/id = :ccts/message-schema
  {::pc/input  #{:list/id}
   ::pc/output [{:list/schemas [:sdb/schema-id :schema/name]}]}
  (when (= id :ccts/message-schema)
    (when-let [schema-maps (->>
                            (d/q `[:find ?ent ?name ?topic
                                   :keys sdb/schema-id schema/name schema/topic
                                   :where
                                   [?ent :schema/name  ?name]
                                   [?ent :schema/topic ?topic]
                                   [?ent :schema/type ~id]]
                                 @du/conn)
                            (sort-by :schema/topic)
                            (mapv #(dissoc % :schema/topic))
                            not-empty)]
      {:list/id id
       :list/schemas schema-maps})))

(pc/defresolver message-schema-r [env input] ; THIS ONE WORKS (But...)
  {::pc/output [{:ccts/message-schema [:list/id {:list/schemas [:schema/name :sdb/schema-id]}]}]}
  {:ccts/message-schema {:list/id :ccts/message-schema}})

;;; (pathom-resolve [{[:sdb/schema-id 3569] [{:model/sequence [{:sdb/elem-id [:sp/name :sp/type :sp/minOccurs :sp/maxOccurs]}]}]}])
;;; (pathom-resolve [{[:sdb/schema-id 3569] [{:model/sequence [:sp/name :sp/type]}]}])
;;; (pathom-resolve [{[:sdb/schema-id 3569] [:schema/name]}])
;;; (pathom-resolve [{[:sdb/schema-id 3569] [:model/sequence]}])
(pc/defresolver schema-props-r [env {:sdb/keys [schema-id]}]
  {::pc/input #{:sdb/schema-id}
   ::pc/output [:schema/name :sdb/schema-id :schema/sdo :schema/type :schema/topic
                :schema/subversion :schema/inlinedTypedefs :schema/spec
                {:schema/importedSchemas [:sdb/imported-schema-id]}
                {:model/sequence [:sdb/elem-id]}]}
  (-> (dp/pull @du/conn '[*] schema-id) ; ToDo: could also do the :keys thing on the pull.
      (update :model/sequence (fn [s] (mapv #(-> % (assoc :sdb/elem-id (:db/id %)) (dissoc :db/id)) s)))
      (update :schema/importedSchemas
              (fn [s]
                (mapv #(-> %
                           (assoc :sdb/imported-schema-id (:db/id %))
                           (dissoc :db/id)) s)))))

;;; (pathom-resolve [{[:sdb/elem-id 1280] [:schema/min-occurs]}])                          ; Simple
;;; (pathom-resolve [{[:schema/name invoice] [{[:sdb/elem-id 1280] [:schema/min-occurs]}]}]) ; YES!
;;; (pathom-resolve [{[:schema/name invoice] [{:model/sequence [:sp/name :sp/type :sp/minOccurs :sp/maxOccurs]}]}]) ; COMPLETE!
;;; (pathom-resolve [{[:sdb/schema-id 1230] [{:model/sequence [:sp/name :sp/type :sp/minOccurs :sp/maxOccurs]}]}]) ; COMPLETE!
(pc/defresolver elem-props-r [env {:sdb/keys [elem-id]}]
  {::pc/input #{:sdb/elem-id}
   ::pc/output [:doc/docString
                :sp/name
                :sp/type
                :sp/minOccurs
                :sp/maxOccurs]}
  (dp/pull @du/conn '[*] elem-id))

;;; Nice thing about pathom (relative to GraphQL) is that you don't have to start at the root.
;;; This has nothing to do with ::pc/input; you can add this to a query anywhere.
(pc/defresolver current-system-time-r [_ _]
  {::pc/output [:server/time]}
  {:server/time (java.util.Date.)})

;;; ToDo: Of course, this needs to take an argument or be part of a user's project, etc.
;;; (pathom-resolve [{[:file/id :map-spec] [:user/data-file]}])
(pc/defresolver data-file-r [env {:file/keys [id]}]
  {::pc/input #{:file/id}
   ::pc/output [:file/text]}
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
  (if-let [owning-schema (d/q `[:find ?s :where [?s :model/sequence ~elem-id]] @du/conn)]
    (let [ref-1 (d/q `[:find [?rs ...] :where [~owning-schema :schema/importedSchemas ?rs]] @du/conn)
          ref-2 (d/q `[:find [?rs ...] :where [~owning-schema :schema/inlinedTypedefs  ?rs]] @du/conn)
          _refs (into ref-1 ref-2)
          _elem-info (dp/pull @du/conn '[*] elem-id)]
      (if-let [_ref-schema nil #_(schema-containing-ref)]
        :found-it
        (log/warn "Could not find referenced schema for" elem-id)))
    (log/warn "Could not find owning schema for" elem-id)))

(def resolvers [schema-by-name-r
                full-schema-r
                schema-props-r
                elem-props-r
                list-r
                message-schema-r
                data-file-r
                current-system-time-r])

(def parser
  (p/parser
   {::p/env     {::p/reader               [p/map-reader
                                           pc/reader2
                                           pc/open-ident-reader
                                           p/env-placeholder-reader]
                 ::p/placeholder-prefixes #{">"}}
    ::p/mutate  pc/mutate
    ::p/plugins [(pc/connect-plugin {::pc/register resolvers})
                 p/error-handler-plugin
                 p/trace-plugin]}))

(defn pathom-resolve
  "Run the pathom parser on the path and return results."
  [query]
  (parser {} query))
