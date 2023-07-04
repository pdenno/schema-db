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
;;; In the process, keep track of the sp's :sp*_children, a property that is not in the DB owing to
;;; how much repetitive content that would produce for most schema and profiles.
;;; The idea of the expand methods is to serve client queries *indirectly* from DB content.
(defn inlined-typedef-ref
  "Return the object defining the argument type-term in the argument schema
  using inlined schema."
  [type-term schema-urn]
  (when-let [ent (d/q `[:find ?ent . :where
                          [?s :schema_name ~schema-urn]
                          [?s :model_inlinedTypedef ?ent]
                          [?ent :sp_type ~type-term]]
                      @(connect-atm))]
    (-> (du/resolve-db-id {:db/id ent} (connect-atm))
        (assoc :mm_access-method :inlined-typedef))))

(defn imported-typedef-ref
  "Return the object defining the argument type-term in the argument schema
   using imported schema."
  [type-term schema-urn]
  (let [[prefix term] (clojure.string/split type-term #":")]
    (when (and term prefix) ; Without the prefix things go awry!
      (let [[{:keys [mm_ent mm_lib]}] ; 4th
            (d/q `[:find ?ref2 ?s2-name
                   :keys mm_ent mm_lib
                   :where
                   [?s1    :schema_name ~schema-urn]
                   [?s1    :schema_importedSchemas ?i]
                   [?i     :import_prefix ~prefix]
                   [?i     :import_referencedSchema ?s2-name]
                   [?s2    :schema_name ?s2-name]
                   [?s2    :schema_content ?ref1] ; many :db/id
                   [?ref1  :sp_name ~term]
                   [?ref1  :sp_function ?fn]
                   [?fn    :fn_type :type-ref]
                   [?ref1  :sp_type ?type]
                   [?s2    :schema_content ?ref2]
                   [?ref2  :sp_name ?type]]
                 @(connect-atm))]
        (when (and mm_ent mm_lib)
          (-> (du/resolve-db-id {:db/id mm_ent} (connect-atm))
              (assoc :mm_lib-where-found mm_lib)
              (assoc :mm_access-method :imported-typedef)))))))

(defn model-sequence-type-ref
  [type-term schema-urn]
  (when-let [ent (d/q `[:find ?m . :where
                        [?s  :schema_name ~schema-urn]
                        [?s  :model_sequence ?m]
                        [?m  :sp_type ~type-term]]
                      @(connect-atm))]
    (-> (du/resolve-db-id {:db/id ent} (connect-atm))
        (assoc :mm_access-method :model-sequence-type))))

(defn schema-ref
  "The term is found in the :model_sequence of a schema; return the schema."
  [term schema-urn]
  (when-let [ent (d/q `[:find ?s . :where
                        [?s  :schema_name ~schema-urn]
                        [?s  :schema_type :ccts_message-schema]
                        [?s  :model_sequence ?m]
                        [?m  :sp_name ~term]
                        [?m  :sp_type ?type]]
                      @(connect-atm))]
    (let [found (du/resolve-db-id {:db/id ent} (connect-atm))]
      (-> {} ; I'm not keeping much of the schema!
          (assoc :db/id          ent)
          (assoc :schema_type    :ccts_message-schema)
          (assoc :model_sequence (:model_sequence found))
          (assoc :mm_access-method :schema-ref)))))

(defn included-typedef-ref ; This is just for OAGIS, AFAIK.
  "Return the object defining the argument type-term in the argument schema
   using included schema."
  [type-term schema-urn]
  (when-let [ent (d/q `[:find ?s2 #_?cont . :where
                        [?s1 :schema_name ~schema-urn]
                        [?s1 :schema_includedSchemas ?i]
                        [?s2 :schema_name ?i]
                        [?s2 :sp_name ~type-term]
                        #_[?cont  :term_type ~type-term]]
                      @(connect-atm))]
    (-> (du/resolve-db-id {:db/id ent} (connect-atm))
        (assoc :mm_access-method :included-typedef))))

;;;(library-lookup-ref "UBLExtension" "urn:oasis:names:specification:ubl:schema:xsd:CommonExtensionComponents-2")
(defn library-lookup-ref
  "Find the term as :schema_content"
  [term schema-urn]
  (when-let [ent (d/q `[:find ?c . :where
                        [?s :schema_name ~schema-urn]
                        [?s :schema_content ?c]
                        [?c :sp_name ~term]]
                      @(connect-atm))]
    (-> (du/resolve-db-id {:db/id ent} (connect-atm))
        (assoc :mm_access-method :library-lookup))))

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
  (cond (= (:mm_access-method found) :library-lookup) :type-def
        (s/valid? ::ccts-based-message-schema found)  :ccts_message-schema
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
                              (assoc :sp_type (:sp_name res))
                              (assoc :sp_name term)
                              #_(dissoc :mm_access-method))
               :type-ref  (-> (expand-aux obj (:sp_type res) schema)
                               (assoc :expand/method ::type-ref)
                               (assoc :sp_name (:sp_name res)))
               :ccts_message-schema (-> obj ; 4th
                                    (assoc :expand/method :ccts_message-schema)
                                    (assoc :sp_name term)
                                    (assoc :sp_children (mapv #(expand-aux {} (:sp_type %) schema)
                                                              (:model_sequence res))))
               :model-seq (-> obj
                               (assoc :expand/method ::model-seq)
                               (assoc :sp_name term)
                               (assoc :sp_children (mapv #(expand-aux {} (:sp_name %) schema)
                                                         (:model_sequence res))))
               nil (log/warn "Cannot expand term" term "for schema" schema))))]
    (expand-aux {} term schema)))

(defn sp-defaults
  [sp]
  (cond-> sp
    (not (:sp_minOccurs sp)) (assoc :sp_minOccurs 1)
    (not (:sp_maxOccurs sp)) (assoc :sp_maxOccurs 1)
    (not (:sp_type       sp)) (assoc :sp_type :mm_undefined)))

;;;=========================================================================================================
(defn get-term-type
  "Return a map describing what is known about the argument data type.
    - Schema-urn is a string
    - term is a string naming a schema object such as a data type (e.g. 'AmountType')"
  [schema-urn term]
  (when-let [ent (or (d/q `[:find ?content .
                            :where
                            [?schema  :schema_name ~schema-urn]
                            [?schema  :schema_content ?content]
                            [?content :term_type ~term]
                            [?content :sp_function ?fn]
                            [?fn      :fn_componentType :ABIE]]
                          @(connect-atm)) ; ToDo: Datahike OR an NOT queries not implemented??? Use predicate?
                     (d/q `[:find ?content .
                            :where
                            [?schema  :schema_name ~schema-urn]
                            [?schema  :schema_content ?content]
                            [?content :term_type ~term]
                            [?content :sp_function ?fn]
                            [?fn      :fn_componentType :BBIE]]
                          @(connect-atm)))]
    (du/resolve-db-id (dp/pull @(connect-atm) '[*] ent) (connect-atm))))
