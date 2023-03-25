(ns schema-db.std-schema
  "Code for common concepts of CCT messaging schema."
  (:require
   [clojure.pprint              :refer [cl-format]]
   [schema-db.db-util           :as du     :refer [connect-atm xpath xpath- xml-type?]]
   [schema-db.schema            :as schema :refer [db-schema+ cct-tag2db-ident-map cct-obj?]]
   [schema-db.schema-util       :as su     :refer [xml-group-by *prefix* merge-warn xsd-attr-map singleize]]
   [schema-db.generic-schema    :as gs  :refer [defparse rewrite-xsd imported-schemas]]
   [taoensso.timbre             :as log]))

(def diag (atom nil))

(defparse :generic/library-schema
  [xmap]
  (binding [*prefix* "type-3"] ;<========================
    (rewrite-xsd xmap :generic/xsd-file)))

(defparse :std/message-schema
  [xmap]
  (binding [*prefix* "model"]
    (let [[_ short-name?] (re-matches  #"[\w,\:\d]+\:(\w+)\-\d" (:schema/name xmap))
          {:keys [import other attrs]} (xml-group-by (xpath xmap :xsd/schema) :xsd/import)
          attrs-map? (-> (xsd-attr-map attrs "schema") not-empty)]
      (cond-> xmap
        short-name?  (assoc :schema/shortName short-name?)
        attrs-map?   (assoc :schema/attributes attrs-map?)
        import       (assoc :schema/importedSchema (imported-schemas import (:xml/ns-info xmap)))
        other        (assoc :schema/content (->> other (mapv rewrite-xsd) singleize))
        true         (dissoc :xml/ns-info :xml/content)))))

(defparse :xsd/element
  [xelem]
  (assert (and (xml-type? xelem :xsd/element)
               (or (-> xelem :xml/attrs :ref)
                   (-> xelem :xml/attrs :name))))
  (binding [*prefix* "element"]
    (let [attrs   (:xml/attrs xelem)
          attrs-map (xsd-attr-map attrs "element")
          content? (->> xelem :xml/content (mapv rewrite-xsd) not-empty)
          typ (if (:name attrs) :model/elementDef :model/elementRef)]
      {typ
       (if content?
         (merge-warn (into [attrs-map] content?))
         attrs-map)})))

;;; Some of these (see UBL-UnqualifiedDataTypes-2.3.xsd) have useful content in :xsd/documentation.
;;; Toplevels for some schema:
;;;   "urn:un:unece:uncefact:data:specification:CoreComponentTypeSchemaModule:2"
;;;   "urn:oasis:names:specification:ubl:schema:xsd:UnqualifiedDataTypes-2"
;;;    OAGIS Components/Fields.xsd
(declare uq-dt-common q-dt-common)

(defparse :generic/unqualified-dtype-schema [xmap {:skip-doc-processing? true}]
  (binding [*prefix* "element"]
    (let [content (-> (xpath xmap :xsd/schema) :xml/content)
          elems   (not-empty (filter #(xml-type? % :xsd/element) content))
          types   (not-empty (filter #(xml-type? % :xsd/complexType) content))]
      (cond-> xmap
        elems (assoc  :schema/content (mapv rewrite-xsd elems))
        types (update :schema/content into (mapv uq-dt-common types))
        true (dissoc :xml/ns-info :xml/content)))))

(defparse :generic/qualified-dtype-schema [xmap]
  (binding [*prefix* "component"]
    (let [{:keys [import other attrs]} (xml-group-by (xpath xmap :xsd/schema) :xsd/import)
          attrs-map? (-> (xsd-attr-map attrs "component") not-empty)]
      (cond-> xmap
        attrs-map?   (assoc :schema/attributes attrs-map?)
        other        (assoc :schema/content (mapv q-dt-common
                                                  (filter #(or (xml-type? % :xsd/complexType) ; ToDo: Is that really it?
                                                               (xml-type? % :xsd/element))
                                                          other)))
        import       (assoc :schema/importedSchema (imported-schemas import (:xml/ns-info xmap)))
        true         (assoc :mm/comment "The :schema/imported-schema is used to lookup the type being restricted.")
        true         (dissoc :xml/ns-info :xml/content))))) ; ToDo: remove :xml/content from this dissoc if you aren't using :schema/content

(defparse :oasis/component-schema
  [xmap]
  (binding [*prefix* "component"]
    (let [{:keys [import attrs]} (xml-group-by (xpath xmap :xsd/schema) :xsd/import)
          attrs-map? (-> (xsd-attr-map attrs "component") not-empty)
          topic? (-> xmap :schema/name su/schema-topic) ; not set yet
          elems (->> (xpath xmap :xsd/schema)
                     :xml/content
                     (filter #(xml-type? % :xsd/element))
                     not-empty)
          comps (->> (xpath xmap :xsd/schema)
                     :xml/content
                     (filter #(xml-type? % :xsd/complexType))
                     not-empty)]
      (cond-> xmap
        topic?       (assoc  :schema/topic topic?)
        true         (assoc  :schema/content [])
        attrs-map?   (assoc  :schema/attributes attrs-map?)
        import       (assoc  :schema/importedSchema (imported-schemas import (:xml/ns-info xmap)))
        elems        (update :schema/content into (mapv rewrite-xsd elems))
        comps        (update :schema/content into
                             (mapv (fn [cplx]
                                     (as-> (rewrite-xsd cplx :xsd/complexType) ?t
                                       (if (:sp/type ?t) (assoc ?t :term/type (:sp/type ?t)) ?t)
                                       (dissoc ?t :sp/type)))
                                   comps))
        true        (dissoc :xml/ns-info :xml/content)))))

(defparse :generic/code-list-schema
  [xmap]
  (binding [*prefix* "codeList"]
    (let [content (-> (xpath xmap :xsd/schema) :xml/content)]
      (as-> xmap ?x
        (if (= (:schema/name ?x) "urn:oagis-10.6:CodeLists_1.xsd")
          (do ; ToDo: This one is a combination of all the others. I don't yet see the point.
            (log/warn "Skipping CodeLists_1.xsd")
            (assoc ?x :mm/debug "Skipping Code List that is an aggregate of others."))
          (assoc ?x :schema/codeList (mapv #(rewrite-xsd % :generic/code-list)
                                           (filter #(xml-type? % :xsd/simpleType) content))))
        (dissoc ?x :xml/ns-info :xml/content)))))

(defparse :iso/iso-20022-schema
  ;; Translate an ISO 20022 schema, financial codes as simple and complex types.
  [xmap]
  (binding [*prefix* "codeList"]
    (let [content (-> (xpath xmap :xsd/schema) :xml/content)]
      (-> xmap
          (assoc :schema/content
                 (mapv #(rewrite-xsd % :xsd/simpleType)
                       (filter #(xml-type? % :xsd/simpleType) content)))
          (update :schema/content ; ToDo: nils an uninvestigated problem in pain files.
                  conj
                  (filterv identity
                           (mapv #(rewrite-xsd % :xsd/complexType)
                                 (filter #(xml-type? % :xsd/complexType) content))))
          (dissoc :xml/content :xml/ns-info)))))

;;;--------------------- Details (not files) ------------------------------------------
;;; Process any documentation or annotation to whatever form is most appropriate.
;;; I think :xsd/sequence inside the annotation (providing more :xsd/documents) can be merged,
;;; but that probably needs more investigation.
(defparse :any/doc
  [xmap]
  (when-let [note (xpath- xmap :xsd/annotation)]     (rewrite-xsd note))
  (when-let [doc  (xpath- xmap :xsd/documentation)]  (rewrite-xsd doc)))

;;; An :xsd/annotation can contain multiple :xsd/documentation.
;;; An :xsd/documentation can contain multiple CCT-specific element, e.g. ccts_BasedASCCPRevisionNumber.
;;;    [[#:cct{:BusinessContext [#:cct{:GUID "oagis-id-1d68712f1ff44355bb6b43e2d1862484"} #:cct{:Name "Bc_1"}]}
;;;      [{:cct/ASCCP_GUID "oagis-id-7814973e3d19488ba917cb61ac28b257"}
;;;       {:cct/ASCCPRevisionNumber 1}
;;;       {:cct/ASCCPDefinition
(def cct-obj-plus? (-> cct-obj? (conj :has/docString) set))
(defn collect-docs
  "argument is rewritten nested vector of documentation.
   Unnest and warn if object content isn't from CCT."
  [node]
  (let [docs (atom [])]
    (letfn [(cd [obj]
              (cond (and (map? obj) (some cct-obj-plus? (keys obj))) (swap! docs conj obj),
                    (vector? obj) (doall (map cd obj))
                    (string? obj) (swap! docs conj obj)
                    ;; The when because sometimes we see something like this: <ccts_Definition />.
                    :else (when obj (log/warn "Unknown doc" obj))))]
      (cd node)
      @docs)))

;;; An annotation can contain multiple :xsd/documentation.
(defparse :xsd/annotation
  [xmap]
  (let [{:keys [documentation other attrs]} (xml-group-by xmap :xsd/documentation)
        typ (if (every? string? documentation) :has/docString :has/documentation) ; ToDo: Not really sufficient.
        docs (singleize documentation)
        docs (if (vector? docs)
               (mapv rewrite-xsd docs)
               (rewrite-xsd docs))
        docs (collect-docs docs)]
      (when (or (not-empty other) (not-empty attrs))
        (log/warn "Annotation contains more than xsd:documentation: " {:other other :attrs attrs}))
      (if (-> docs first vector?) ; Not investigated /opt/messaging/sources/misc/elena/2023-02-09/ProcessInvoice-BC_1.xsd
        {typ (first docs)}
        {typ docs})))

;;; Typically a documentation will contain many CCT elements.
(defparse :xsd/documentation
  [xmap]
  (let [content (:xml/content xmap)]
    (if (string? content)
      {:has/docString content}
      (->> content (mapv rewrite-xsd) singleize))))

(defn simple-cct
  "Process all forms of CCT special tags. (See schema.clj.)"
  [db-ident xmap]
  (let [content? (-> xmap :xml/content not-empty)
        typ (-> db-schema+ db-ident :db/valueType)
        readable? (#{:db.type/long :db.type/float :db.type/number, :db.type/double :db.type/boolean} typ)]
    (if (#{:cct/Component
           :cct/BusinessContext ; These are containers objects.
           :cct/ContentComponentValueDomain} db-ident)
      {db-ident (merge-warn (mapv rewrite-xsd (:xml/content xmap)))}
      (cond-> {}
        content?                      (assoc db-ident content?)
        (-> xmap :xml/attrs :source)  (assoc :has/source (-> xmap :xml/attrs :source))
        readable?                     (update db-ident read-string)
        (= typ :db.type/keyword)      (update db-ident keyword)))))

;;; OAGIS uses this one.
(defparse :generic/simple-cct
  [xmap]
  (when (-> xmap :xml/content not-empty)
    (if-let [db-ident (get cct-tag2db-ident-map (:xml/tag xmap))]
      (simple-cct db-ident xmap)
      (log/warn "Unknown CCT (1): " (:xml/tag xmap)))))

;;; OASIS files use ccts as a tag namespace.
(defparse :oasis/component-def
  [xmap]
  (when (-> xmap :xml/content not-empty)
    (let [db-ident (->> xmap :xml/tag name (keyword "cct"))]
      (if (contains? db-schema+ db-ident)
        (simple-cct db-ident xmap)
        (log/warn "Unknown CCT (2): " db-ident)))))

;;; This is for processing the :xsd/documentation under the simpleContent (supplemental component)
;;; under complexContent the :xsd/documentation has the main component.
(defparse :cct/supplementary
  [xmap]
  (assert (xml-type? xmap :xsd/simpleContent))
  (let [res?  (xpath- xmap :xsd/restriction)
        ext?  (xpath- xmap :xsd/restriction)
        attr  (xpath- (or res? ext?) :xsd/attribute)
        name  (-> attr :xml/attrs :name)
        type  (-> attr :xml/attrs :type)
        use   (-> attr :xml/attrs :use)
        doc?  (rewrite-xsd xmap :any/doc)]
    (cond-> {}
      name (assoc :cct/scId   name)
      type (assoc :cct/scType type)
      use  (assoc :cct/scUse  use)
      doc? (assoc :cct/supplementary doc?))))

;;; (def ubl-udt-file "/Users/pdenno/Documents/specs/OASIS/UBL-2.3/xsdrt/common/UBL-UnqualifiedDataTypes-2.3.xsd")
;;; (-> ubl-udt-file read-clean rewrite-xsd)
;;; (6) This should return something. (:schema/def)
(defn uq-dt-common
  "Process common parts of CEFACT and OASIS unqualified datatypes, which are mostly CCT-based documentation."
  [cplx-type]
  (reset! diag {:cplx-type cplx-type})
  (assert (xml-type? cplx-type  :xsd/complexType)) ; ToDo: top part is similar to :xsd/inline-typedef
  (rewrite-xsd cplx-type)
  #_(let [doc?  (let [doc (xpath- cplx-type :xsd/annotation :xsd/documentation)]
                (when-not (-> doc :xml/content string?) doc))
        sup?  (xpath- cplx-type :xsd/simpleContent)
        elems (not-empty (filter #(xml-type? % :xsd/element)
                                 (:xml/content (xpath cplx-type :xsd/sequence))))]
    (cond-> {}
      true  (assoc :temp/typeDef (-> cplx-type :xml/attrs :name)),
      doc?  (assoc :sp/component     (rewrite-xsd doc? :cct/component-doc))
      sup?  (assoc :sp/supplementary (rewrite-xsd sup? :cct/supplementary))
      elems (assoc :temp/sequence (mapv rewrite-xsd elems)))))

;;; ToDo: maybe rewrite this to have something like namespace for the references "udt". "schema/referenced-schema
(defn q-dt-common
  "Process one qualified datatype schema element."
  [xmap]
  (assert (or (xml-type? xmap :xsd/complexType)
              (xml-type? xmap :xsd/element)))
  (let [name (-> xmap :xml/attrs :name)]
    (-> xmap
        rewrite-xsd
        (assoc :term/name name)
        (dissoc :sp/name))))

(defparse :xsd/type-attr
  [xsd-attr]
  (let [attr (:xml/attrs xsd-attr)]
    (cond-> {}
      true         (assoc :sp/name (:name attr))
      (:type attr) (assoc :sp/type (:type attr))
      (= (:use attr) "optional") (assoc :sp/minOccurs :0)
      (= (:use attr) "optional") (assoc :sp/maxOccurs :1))))

;;; (5) like 6, why not :model/typeDef. :temp/sequence won't cut it here.
(defparse :xsd/inline-typedef ; OAGIS only AFAIK
  [cplx-type]
  (assert (xml-type? cplx-type :xsd/complexType))
  (let [ext?  (xpath- cplx-type :xsd/complexContent :xsd/extension)
        res?  (xpath- cplx-type :xsd/complexContent :xsd/restriction)
        attrs (not-empty (filter #(xml-type? % :xsd/attribute) cplx-type))
        elems (not-empty
               (cond ext?  (filter #(xml-type? % :xsd/element)
                                  (:xml/content (xpath ext? :xsd/sequence)))
                     res?  (filter #(xml-type? % :xsd/element)
                                  (:xml/content (xpath res? :xsd/sequence)))
                     :else (filter #(xml-type? % :xsd/element)
                                  (:xml/content (xpath cplx-type :xsd/sequence)))))]
    (cond-> {}
      true    (assoc :sp/type (-> cplx-type :xml/attrs :name)),
      true    (assoc :fn/type :sequence),
      ext?    (assoc :fn/type :extension),
      ext?    (assoc :fn/base (-> ext? :xml/attrs :base))
      res?    (assoc :fn/type :restriction)
      res?    (assoc :fn/base (-> res? :xml/attrs :base))
      attrs   (assoc :schema/type-attrs (mapv #(rewrite-xsd % :xsd/type-attr) attrs)),
      elems   (assoc :temp/sequence (mapv #(rewrite-xsd % :xsd/element) elems)))))

#_(defparse :generic/code-term
  ;; Return a map of the CCTS and ISO properties defined by an OAGIS or OAGIS/ISO code term.
  [xmap {:skip-doc-processing? true}]
  (assert (xml-type? xmap :xsd/enumeration))
  (let [term (-> xmap :xml/attrs :value)
        doc    (xpath xmap :xsd/annotation :xsd/documentation)]
    ;; Some code lists, like OAGIS CodeList_CurrencyCode_ISO_7_04, don't have documentation.
    {term (generic-rewrite doc) #_(rewrite-xsd (:xml/content doc) :oagis/cct-def)}))

#_(defparse :oagis/cct-def
  ;; Argument is a vector of :ROOT/ccts_whatever properties.
  ;; Return a map of these translated :cct/whatever.
  [ccts-tags]
  (reduce (fn [m elem]
            (let [tag (:xml/tag elem)]
              (if-let [ccts-tag (oagis2ccts-key-map tag)]
                (if-let [content (:xml/content elem)]
                  (assoc m ccts-tag content)
                  m)
                (if (= tag :ROOT/CodeName)
                  (assoc m :iso/CodeName (:xml/content elem))
                  (do (log/warn "Unknown code-term tag" tag) m)))))
          {}
          ccts-tags))

(defparse :generic/code-list
  ;; Walk through a code list collecting terms.
  [xmap]
  (assert (xml-type? xmap :xsd/simpleType))
  (reset! diag xmap)
  (let [name?  (-> xmap :xml/attrs :name)
        id?    (-> xmap :xml/attrs :id)
        rest?  (when-let [res (xpath xmap :xsd/restriction)] (-> res rewrite-xsd not-empty))
        ;; I'm assuming they all look like this: #:xml{:tag :xsd/enumeration, :attrs {:value "Add"}}
        terms? (-> (mapv #(-> % :xml/attrs :value)
                         (filterv #(xml-type? % :xsd/enumeration)
                                  (:xml/content (xpath xmap :xsd/restriction)))) not-empty)
        union?  (when-let [res (xpath xmap :xsd/union)] (-> res rewrite-xsd not-empty))
        content (cond-> {}
                  name?  (assoc :codeList/name name?)
                  id?    (assoc :codeList/id id?)
                  rest?  (assoc :codeList/restriction rest?)
                  terms? (assoc :codeList/terms terms?)
                  union? (assoc :codeList/union union?))]
    {:model/codeList content}))

(def non-standard-oagis-schema-topics
  (let [pat {"urn:oagis-~A:CodeList_ConstraintTypeCode_1.xsd"            "Codelist, ConstraintTypes",
             "urn:oagis-~A:CodeList_TimeFormatCode_1.xsd"                "Codelist, TimeFormats"
             "urn:oagis-~A:CodeList_DateTimeFormatCode_1.xsd"            "Codelist, DateTimeFormats"
             "urn:oagis-~A:CodeList_TimeZoneCode_1.xsd"                  "Codelist, TimeZones",
             "urn:oagis-~A:CodeList_DateFormatCode_1.xsd"                "Codelist, DateFormat",
             "urn:oagis-~A:CodeList_CharacterSetCode_IANA_20131220.xsd"  "Codelist, CharacterSets",
             "urn:oagis-~A:CodeLists_1.xsd"                              "Codelist, Aggregated",
             "urn:oagis-~A:CodeList_ConditionTypeCode_1.xsd"             "Codelist, ConditionTypes",
             "urn:oagis-~A:CodeList_CurrencyCode_ISO_7_04.xsd"           "Codelist, Currencies"}]
    (merge (reduce-kv (fn [m k v] (assoc m (cl-format nil k "10.6") v)) {} pat)
           (reduce-kv (fn [m k v] (assoc m (cl-format nil k "10.8") v)) {} pat))))

(def non-standard-schema-topics
  "Easiest to just define these explicitly"
  (merge non-standard-oagis-schema-topics
         {"urn:iso:std:iso:20022:tech:xsd:pain.001.001.04"                              "Datatypes, Financial",
          "urn:iso:std:iso:20022:tech:xsd:pain.001.001.05"                              "Datatypes, Financial",
          "urn:iso:std:iso:20022:tech:xsd:pain.002.001.04"                              "Datatypes, Financial",
          "urn:iso:std:iso:20022:tech:xsd:pain.002.001.05"                              "Datatypes, Financial",
          "urn:iso:std:iso:20022:tech:xsd:pain.008.001.03"                              "Datatypes, Financial",
          "urn:iso:std:iso:20022:tech:xsd:pain.008.001.04"                              "Datatypes, Financial",
          "urn:oasis:names:specification:ubl:schema:xsd:CommonExtensionComponents-2"    "Components, CommonExtensions",
          "urn:oasis:names:specification:ubl:schema:xsd:UnqualifiedDataTypes-2"         "Datatypes, Unqualified",
          "urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2"        "Components, CommonBasic",
          "urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2"    "Components, CommonAggregate",
          "urn:un:unece:uncefact:data:specification:CoreComponentTypeSchemaModule:2"    "Components, Core",
          "urn:oasis:names:specification:ubl:schema:xsd:QualifiedDataTypes-2"           "Datatypes, Qualified"
          "urn:oasis:names:specification:ubl:schema:xsd:CommonSignatureComponents-2"    "Components, CommonSignature",
          "http://uri.etsi.org/01903/v1.4.1#"                                            "ETSI, not investigaated"}))

(defn schema-topic
  "Return the portion of the URN that most specifically describes the schema content.
   This is not necessarily unique!"
  [urn]
  (let [desc [(su/q-schema-sdo urn) (su/q-schema-type urn)]]
     (cond (= desc [:oasis :ccts/message-schema])
           (->> urn (re-matches #"^urn:oasis:names:specification:ubl:schema:xsd:(.+)-\d$") second),

           (= desc [:oagi :ccts/message-schema])
           (->> urn (re-matches #"^urn:oagis-\d+\.\d+:(.+)$") second),

           (= desc [:oagi :generic/code-list-schema])
           (->> urn (re-matches #"^urn:oagis-\d+\.\d+:(.+)$") second),

           (= desc [:qif :generic/message-schema])
           (->> urn (re-matches #"^urn:QIF-\d:Application:QIF(.+)$") second),

           (= desc [:qif :generic/library-schema])
           (->> urn (re-matches #"^urn:QIF-\d:Library:(.+)$") second),

           (contains? non-standard-schema-topics urn)
           (get non-standard-schema-topics urn),

           :else
           (do (log/warn "Cannot determine schema topic" urn) ""))))

;;;========================================= WIP =======================================
;;; For debugging
(defn query-for [pathom-eql] ((var-get (resolve 'rad-mapper.pathom/parser)) {} pathom-eql))

(defn mf-spec
  "Get Michael's TestSpecification Schema"
  []
  (let [sname "urn:oagis-10.6:TestSpecification-MF"]
    (as-> sname ?s
      (query-for [{[:schema/name ?s] [:sdb/schema-id]}])
      (get ?s [:schema/name sname])
      (:sdb/schema-id ?s)
      {:db/id ?s}
      (du/resolve-db-id ?s (connect-atm)))))

(defn mf-meth
  "Get Michael's TestMethod Schema"
  []
  (query-for [{[:schema/name "urn:oagis-10.6:TestMethod-MF"] [:sdb/schema-id]}]))
