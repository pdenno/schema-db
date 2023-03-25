(ns schema-db.schema-lost
  (:require
   [clojure.spec.alpha  :as s]
   [clojure.string      :as str]
   [datahike.api        :as d]
   [datahike.pull-api   :as dp]
   [schema-db.db-util   :as du :refer [connect-atm]]
   [taoensso.timbre     :as log]))

;;;================================ Are these being used? ======================================
;;; ================================ Expand: resolve to atomic schema parts  ===========================
;;; In the process, keep track of the sp's :sp*/children, a property that is not in the DB owing to
;;; how much repetitive content that would produce for most schema and profiles.
;;; The idea of the expand methods is to serve client queries *indirectly* from DB content.
(defn inlined-typedef-ref
  "Return the object defining the argument type-term in the argument schema
  using inlined schema."
  [type-term schema-urn]
  (when-let [ent (d/q `[:find ?ent . :where
                          [?s :schema/name ~schema-urn]
                          [?s :model/inlinedTypedef ?ent]
                          [?ent :sp/type ~type-term]]
                      @(connect-atm))]
    (-> (du/resolve-db-id {:db/id ent} (connect-atm))
        (assoc :mm/access-method :inlined-typedef))))

(defn imported-typedef-ref
  "Return the object defining the argument type-term in the argument schema
   using imported schema."
  [type-term schema-urn]
  (let [[prefix term] (clojure.string/split type-term #":")]
    (when (and term prefix) ; Without the prefix things go awry!
      (let [[{:mm/keys [ent lib]}]
                (d/q `[:find ?ref2 ?s2-name
                       :keys mm/ent mm/lib
                       :where
                       [?s1    :schema/name ~schema-urn]
                       [?s1    :schema/importedSchemas ?i]
                       [?i     :import/prefix ~prefix]
                       [?i     :import/referencedSchema ?s2-name]
                       [?s2    :schema/name ?s2-name]
                       [?s2    :schema/content ?ref1] ; many :db/id
                       [?ref1  :sp/name ~term]
                       [?ref1  :sp/function ?fn]
                       [?fn    :fn/type :type-ref]
                       [?ref1  :sp/type ?type]
                       [?s2    :schema/content ?ref2]
                       [?ref2  :sp/name ?type]]
                     @(connect-atm))]
        (when (and ent lib)
          (-> (du/resolve-db-id {:db/id ent} (connect-atm))
              (assoc :mm/lib-where-found lib)
              (assoc :mm/access-method :imported-typedef)))))))

(defn model-sequence-type-ref
  [type-term schema-urn]
  (when-let [ent (d/q `[:find ?m . :where
                        [?s  :schema/name ~schema-urn]
                        [?s  :model/sequence ?m]
                        [?m  :sp/type ~type-term]]
                      @(connect-atm))]
    (-> (du/resolve-db-id {:db/id ent} (connect-atm))
        (assoc :mm/access-method :model-sequence-type))))

(defn schema-ref
  "The term is found in the :model/sequence of a schema; return the schema."
  [term schema-urn]
  (when-let [ent (d/q `[:find ?s . :where
                        [?s  :schema/name ~schema-urn]
                        [?s  :schema/type :ccts/message-schema]
                        [?s  :model/sequence ?m]
                        [?m  :sp/name ~term]
                        [?m  :sp/type ?type]]
                      @(connect-atm))]
    (let [found (du/resolve-db-id {:db/id ent} (connect-atm))]
      (-> {} ; I'm not keeping much of the schema!
          (assoc :db/id          ent)
          (assoc :schema/type    :ccts/message-schema)
          (assoc :model/sequence (:model/sequence found))
          (assoc :mm/access-method :schema-ref)))))

(defn included-typedef-ref ; This is just for OAGIS, AFAIK.
  "Return the object defining the argument type-term in the argument schema
   using included schema."
  [type-term schema-urn]
  (when-let [ent (d/q `[:find ?s2 #_?cont . :where
                        [?s1 :schema/name ~schema-urn]
                        [?s1 :schema/includedSchemas ?i]
                        [?s2 :schema/name ?i]
                        [?s2 :sp/name ~type-term]
                        #_[?cont  :term/type ~type-term]]
                      @(connect-atm))]
    (-> (du/resolve-db-id {:db/id ent} (connect-atm))
        (assoc :mm/access-method :included-typedef))))

;;;(library-lookup-ref "UBLExtension" "urn:oasis:names:specification:ubl:schema:xsd:CommonExtensionComponents-2")
(defn library-lookup-ref
  "Find the term as :schema/content"
  [term schema-urn]
  (when-let [ent (d/q `[:find ?c . :where
                        [?s :schema/name ~schema-urn]
                        [?s :schema/content ?c]
                        [?c :sp/name ~term]]
                      @(connect-atm))]
    (-> (du/resolve-db-id {:db/id ent} (connect-atm))
        (assoc :mm/access-method :library-lookup))))

;;; (term-ref ubl-invoice "InvoiceType")    ; get from inlined.
;;; (term-ref ubl-invoice "cbc:IssueDate")  ; get from imported.
;;; (term-ref ubl-invoice "cac:InvoiceLine") ; get from imported.
;;; (term-ref oagis-invoice "InvoiceLine") ; get from included
(defn term-ref
  "Return the object referenced by the term in schema."
  [term schema-urn]
  (or (library-lookup-ref      term schema-urn)
      (inlined-typedef-ref     term schema-urn)
      (imported-typedef-ref    term schema-urn)
      (included-typedef-ref    term schema-urn)
      (model-sequence-type-ref term schema-urn)
      (schema-ref              term schema-urn)
      #_(model-sequence-name-ref term schema-urn)
      (throw (ex-info (str "In term-ref could not resolve " term " " schema-urn)
                      {:term term :schema-urn schema-urn}))))

;;; ToDo: Remember that once you run this, it is in the schema from then on.
;;; (get-schema "small-invoice-schema-1")
(defn store-test [] (->> "small-invoice-schema-1.edn" slurp read-string vector (d/transact (connect-atm))))
;;; (expand "Invoice" "small-invoice-schema-1")

(defn expand-type
  [found]
  (cond (= (:mm/access-method found) :library-lookup) :type-def
        (s/valid? ::ccts-based-message-schema found)  :ccts/message-schema
        (s/valid? ::tagged found)          :tagged
        (s/valid? ::type-ref found)        :type-ref
        (s/valid? ::model-seq found)       :model-seq
        :else nil))

(defn expand [term schema]
  (letfn [(expand-aux [obj term schema]
            (let [res  (term-ref term schema)
                  type (expand-type res)]
              (println "\n\n term = " term  "\n res =" res "\n type =" type)
              (case type
                :type-def res
                :tagged   (-> res
                              (assoc :expand/method ::tagged)
                              (assoc :sp/type (:sp/name res))
                              (assoc :sp/name term)
                              #_(dissoc :mm/access-method))
               :type-ref  (-> (expand-aux obj (:sp/type res) schema)
                               (assoc :expand/method ::type-ref)
                               (assoc :sp/name (:sp/name res)))
               :ccts/message-schema (-> obj
                                    (assoc :expand/method :ccts/message-schema)
                                    (assoc :sp/name term)
                                    (assoc :sp/children (mapv #(expand-aux {} (:sp/type %) schema)
                                                              (:model/sequence res))))
               :model-seq (-> obj
                               (assoc :expand/method ::model-seq)
                               (assoc :sp/name term)
                               (assoc :sp/children (mapv #(expand-aux {} (:sp/name %) schema)
                                                         (:model/sequence res))))
               nil (log/warn "Cannot expand term" term "for schema" schema))))]
    (expand-aux {} term schema)))

(defn sp-defaults
  [sp]
  (cond-> sp
    (not (:sp/minOccurs sp)) (assoc :sp/minOccurs 1)
    (not (:sp/maxOccurs sp)) (assoc :sp/maxOccurs 1)
    (not (:sp/type       sp)) (assoc :sp/type :mm/undefined)))

;;;=========================================================================================================
(defn get-term-type
  "Return a map describing what is known about the argument data type.
    - Schema-urn is a string
    - term is a string naming a schema object such as a data type (e.g. 'AmountType')"
  [schema-urn term]
  (when-let [ent (or (d/q `[:find ?content .
                            :where
                            [?schema  :schema/name ~schema-urn]
                            [?schema  :schema/content ?content]
                            [?content :term/type ~term]
                            [?content :sp/function ?fn]
                            [?fn      :fn/componentType :ABIE]]
                          @(connect-atm)) ; ToDo: Datahike OR an NOT queries not implemented??? Use predicate?
                     (d/q `[:find ?content .
                            :where
                            [?schema  :schema/name ~schema-urn]
                            [?schema  :schema/content ?content]
                            [?content :term/type ~term]
                            [?content :sp/function ?fn]
                            [?fn      :fn/componentType :BBIE]]
                          @(connect-atm)))]
    (du/resolve-db-id (dp/pull @(connect-atm) '[*] ent) (connect-atm))))
