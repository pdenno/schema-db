(ns schema-db.core
  "Create and populate a database with schemas from various SDOs #{:CEFACT :OASIS :OAGI :ISO}
   Every element (a named BBIE, BIE, data type etc.) has an :schema/term and a :sp/type.
   (sp = schema-part, an abstraction over content and attributes.)
   The :sp/type are #{:BCC :ACC :ASCC :CCT :uDT :qDT :BBIE :ABIE :ASBIE } cite{Kabak2010}.
   :sp are derived from :xsd/element.
   #{:ABIE :BBIE :ASBIE} have various 'contexts' (e.g. the business process context :Trade).
   :uDT and :qDT specify restrictions on :CCT (core component type) and value sets of :BCC.
   Elements are organized into schema.
   Most access is through pathom, but some key functions for the REPL:
     (list-schemas) - Get the list of schema (their :schema/name), which is a URN string.
     (get-schema <schema-urn-string>) - Get a map describing the whole schema. Its elements are :schema/content.
     (get-term <schema-urn-string> <term>) - Get the map of information for the :schema/term.
     (get-term-schemas <term>) - Get a list of schema (their URN strings) for the term (a string)."
  (:require
   [clojure.java.io              :as io]
   [clojure.pprint               :refer [cl-format]]
   [clojure.spec.alpha           :as s]
   [clojure.string               :as str]
   [datahike.api                 :as d]
   [datahike.pull-api            :as dp]
   [mount.core                   :refer [defstate]]
   [schema-db.db-util            :as du    :refer [conn xpath xpath- xml-type?]]
   [schema-db.schema-util        :as su    :refer [special-schema-type? generic-schema-type? q-schema-sdo q-schema-topic q-schema-type]]
   [schema-db.std-schema]
   [schema-db.generic-schema     :as gen-s :refer [rewrite-xsd read-schema-file]]
   [schema-db.util               :as util]
   [taoensso.timbre              :as log]))

;;; ToDo:
;;;    * The best thing to do with docs is alway collect them, but eliminate them in presentation like I optionally do with :db/id.
;;;    * elem-props-r only works for UBL schema.
;;;    * Get :sp/function into everything. BTW, CEFACT schema have  <ccts:Acronym>BBIE</ccts:Acronym>
;;;    * Maybe split this file into something in src/dev and something in src/main/app/server (which is where the pathom stuff is).
;;;      But then I wonder what would remain in src/main/app/model.

(def base-dir
  (or (-> (System/getenv) (get "RM_MESSAGING"))
      (throw (ex-info (str "Set the environment variable RM_DATABASES to the directory in which you want databases written."
                           "\nCreate a directory 'schema' under it.") {}))))

(def db-dir
    (if (-> base-dir (str "/databases/schema") clojure.java.io/file .isDirectory)
      (str base-dir "/databases/schema")
      (throw (ex-info "Directory not found:" {:dir (str base-dir "/databases/schema")}))))

(def src-dir
  (if (-> base-dir (str "/sources") clojure.java.io/file .isDirectory)
        (str base-dir "/sources")
        (throw (ex-info "Directory not found:" {:dir (str base-dir "/sources")}))))

(def db-cfg {:store {:backend :file :path db-dir}
             :rebuild-db? true
             :schema-flexibility :write})

(def diag (atom nil))

;;; ToDo: Make the following environment variables.
(def ubl-root         (str src-dir "/OASIS/UBL-2.3/xsdrt/"))
(def oagis-10-8-root  (str src-dir "/OAGIS/10.8.4/ModuleSet/Model/"))
(def qif-root         (str src-dir "/QIF/3.0/xsd/"))
(def michael-root     (str src-dir "/michaelQIF/"))

(defonce bad-file-on-rebuild? (atom #{})) ; For debugging

;;; NB :db.cardinality/many means the property has a vector value {:foo/foo [1 2 3]}
;;;    :db.type/ref means that the property means the element(s) of the property are db/id to maps.
;;;    Perhaps everything that is the value of a ref should have its own ns name.
(def db-schema
  "Defines the datahike schema for this database.
     :db/db.cardinality=many means value is a vector of values of some :db.type."
  [#:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/Cardinality}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :cct/CategoryCode}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/DataTypeTermName}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/Definition}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/Description}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/DictionaryEntryName}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/Name}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/ObjectClass}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :cct/PrimitiveType}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/PropertyTermName}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/QualifierTerm}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/RepresentationTermName}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/UniqueID}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/UsageRule}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/VersionID}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :cct/scId
        :doc "'sc' is supplementary to component"}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :cct/scType}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :cct/scUse}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :cct/supplementary}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :codeList/name}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :codeList/lists}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :codeList/terms}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :doc/docString}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :fn/componentType}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :fn/type}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :fn/base}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :import/prefix}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :import/referencedSchema}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :iso/CodeName,
        :doc "Used in OAGIS/ISO currency codes"}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :mm/comment
        :doc "All the mm things are for debugging."}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :mm/debug}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/boolean, :ident :mm/fileNotRead?}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :mm/unhandledXML}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,  :ident :mm/tempInclude}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :model/sequence
        :doc "generic modeling concept"}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,  :ident :model/enumeration}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :schema/complexTypes}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :schema/content
        :doc "typically this includes the entire content of an xsd file."}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :schema/importedSchemas}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,  :ident :schema/includedSchemas}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :schema/inlinedTypedefs}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/name, :unique :db.unique/identity}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/pathname}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :schema/sdo}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/shortName}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :schema/simpleTypes}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :schema/spec}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/subversion
        :doc "e.g. for OAGIS 10.8 it is '8'"}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/topic}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :schema/type}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :schema/version
        :doc "e.g. for OAGIS 10.8 it is '10'"}
   ;; sp is 'schema property', general sort of things.
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/boolean, :ident :sp/abstract}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/ref,     :ident :sp/component
        :doc "CCT see also supplementary"}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,  :ident :sp/docString
        :doc "when :xsd/documentation is a string, rather than :sp/supplementary
              ToDo: Should I just use :doc/docString ?"}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/ref,     :ident :sp/function,
        :doc ":fn objects."}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :sp/maxOccurs}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :sp/minOccurs}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :sp/name}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :sp/ref
        :doc "e.g. xsd:element attr."}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/ref,     :ident :sp/supplementary
        :doc "CCT"}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :sp/type}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :sp/typeRef
        :doc "xsd:elem with a name, type attrs"}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/keyword, :ident :sp/xsdType}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :term/name,
        :doc "An ID unique within the schema"}
   ;; ToDo: These are typically things that could be generalized or need further investigation
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :xsd/attributeGroup}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,  :ident :xsdAttrGroup/data}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :xsd/choice}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/long,    :ident :xsd/fractionDigits}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :xsd/listItemType
        :doc "This is the itemType attr of the element."}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/long,    :ident :xsd/length}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/long,    :ident :xsd/maxLength}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/number,  :ident :xsd/minExclusive}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/number,  :ident :xsd/maxExclusive}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/number,  :ident :xsd/minInclusive}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/number,  :ident :xsd/maxInclusive}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/long,    :ident :xsd/minLength}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/string,  :ident :xsd/pattern}
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/ref,     :ident :xsd/restriction} ; ToDo: eliminate this.
   #:db{:cardinality :db.cardinality/one,  :valueType :db.type/long,    :ident :xsd/totalDigits}
   ;; These are for boxing values.
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,  :ident :zip/keys}
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,     :ident :zip/vals}])

;;; NB s/every-kv and s/every are probabilistic; they do not check every entry.
;;; ToDo: Write something that uses these and does d/q to check the whole DB. Of course, these need more work!
(s/def ::db-ent (s/keys :req [:db/id]))
(s/def ::type-ref (s/and ::db-ent (s/keys :req [:sp/name :sp/type])))
(s/def ::tagged (s/or :generic-elem ::gelem :component ::basic))
(s/def ::basic (s/and ::db-ent (s/keys :req [:sp/name :sp/function]) #(= :BBIE  (-> % :sp/function :fn/componentType))))
(s/def ::gelem (s/and ::db-ent (s/keys :req [:sp/name :sp/function]) #(= :gelem (-> % :sp/function :fn/type))))
(s/def ::quantified-elem (s/and ::gelem (s/keys :req [:sp/minOccurs :sp/maxOccurs])))
(s/def ::gelems (s/every ::gelem))
(s/def ::model-seq (s/and ::db-ent (s/keys :req [:model/sequence]) #(s/valid? ::gelems (:model/sequence %)))) ; Test a property!
(s/def ::ccts-based-message-schema (s/and ::db-ent (s/keys :req [:schema/type]) #(= :ccts/message-schema (:schema/type %))))

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
  (let [desc [(q-schema-sdo urn) (q-schema-type urn)]]
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

;;;=======================  Rewrite xsd to schema content for DB  ===========================

(s/def ::schema-type-kw #(or (special-schema-type? %)
                             (generic-schema-type? %)))

;;; Seen once in a QIF file.
(defmethod rewrite-xsd :xsd/attributeGroup
  [xmap]
  (let [doc (-> xmap (xpath- :xsd/annotation :xsd/documentation) :xml/content)]
    {:xsd/attributeGroup
     (cond-> {}
       (not-empty doc)   (assoc :sp/docString doc)
       true              (assoc :xsdAttrGroup/data (-> xmap :xml/attrs :ref)))}))

;;;---------------------------------- 'Custom' ---------------------------------------------------
(def cct-renames
  {:ccts/Cardinality :cct/Cardinality,
   :ccts/CategoryCode  :cct/CategoryCode,
   :ccts/DataTypeTermName :cct/DataTypeTermName,
   :ccts/Definition :cct/Definition,
   :ccts/Description :cct/Description,
   :ccts/DictionaryEntryName :cct/DictionaryEntryName,
   :ccts/Name :cct/Name,
   :ccts/ObjectClass :cct/ObjectClass,
   :ccts/PrimitiveType :cct/PrimitiveType,
   :ccts/PropertyTermName :cct/PropertyTermName,
   :ccts/QualifierTerm :cct/QualifierTerm,
   :ccts/RepresentationTermName :cct/RepresentationTermName,
   :ccts/UniqueID :cct/UniqueID,
   :ccts/UsageRule :cct/UsageRule,
   :ccts/VersionID :cct/VersionID,
   :ccts/sc-id :cct/scId,
   :ccts/sc-type :cct/scType,
   :ccts/sc-use :cct/scUse,
   :ccts/supplemental :cct/supplemental})

(def oagis2ccts-key-map
  "Translate names of properties found in OAGIS to the equivalent used in the database."
  (let [key-names (->> db-schema (map #(get % :db/ident)) (filter #(and % (= (namespace %) "cct"))))
        oagis-names (map #(keyword "ROOT" (str "ccts_" (name %))) key-names)]
    (zipmap oagis-names key-names)))

(defn fix-ccts-keys
  "Some files uses keywords qualified by 'ccts', I use 'cct' everywhere."
  [k]
  (or (cct-renames k) (oagis2ccts-key-map k) k))

(defn cc-from-doc
  "Return a map describe a core component or supplementary component from the
   argument :xsd/documentation."
  [xmap]
  (assert (xml-type? xmap :xsd/documentation))
  (not-empty (reduce (fn [m info]
                       (if-let [content (:xml/content info)]
                         (assoc m (-> info :xml/tag fix-ccts-keys) content)
                         m))
                     {}
                     (:xml/content xmap))))

;;; (def ubl-udt-file "/Users/pdenno/Documents/specs/OASIS/UBL-2.3/xsdrt/common/UBL-UnqualifiedDataTypes-2.3.xsd")
;;; (-> ubl-udt-file read-clean rewrite-xsd)
(defn uq-dt-common
  "Process common parts of CEFACT and OASIS unqualified datatypes.
   These (see UBL-UnqualifiedDataTypes-2.3.xsd are the things with useful
   content in :xsd/documentation."
  [cplx-type]
  (assert (xml-type? cplx-type  :xsd/complexType)) ; ToDo: top part is similar to :inline-typedef
  (let [doc?  (let [doc (xpath- cplx-type :xsd/annotation :xsd/documentation)]
                (when-not (-> doc :xml/content string?) doc))
        sup?  (xpath- cplx-type :xsd/simpleContent)
        elems (not-empty (filter #(xml-type? % :xsd/element)
                                 (:xml/content (xpath cplx-type :xsd/sequence))))]
    (cond-> {}
      true  (assoc :sp/type (-> cplx-type :xml/attrs :name)),
      true  (assoc :sp/function {:fn/type :sequence}),
      doc?  (assoc :sp/function {:fn/type :cct-component}),
      doc?  (assoc :sp/component     (rewrite-xsd doc? :cct-component))
      sup?  (assoc :sp/supplementary (rewrite-xsd sup? :cct-supplementary))
      elems (assoc :model/sequence (mapv rewrite-xsd elems)))))


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

;;;=========================== Schema Operations ===========================================
(defn list-schemas
  "Return a list of schema, by default they are sorted by 'topic'"
  [& {:keys [sdo sort?] :or {sort? true}}]
  (let [base-names
        (if sdo
          (d/q `[:find [?n ...] :where [?s :schema/name ?n] [?s :schema/sdo ~sdo]] @conn)
          (d/q '[:find [?n ...] :where [_ :schema/name ?n]] @conn))]
    (if sort?
      (let [urns&topics (map (fn [urn topic] {:urn urn :topic topic})
                             base-names
                             (map q-schema-topic base-names))]
        (->> urns&topics
             (sort-by :topic)
             (mapv :urn)))
      (vec base-names))))

(defn get-schema
  "Return the map stored in the database for the given schema-urn. Useful in development.
    :filter-set - DB attribute to leave out (e.g. #{:db/id} or #{:db/doc-string}) "
  [schema-urn & {:keys [resolve? filter-set] :or {resolve? true filter-set #{:doc/docString}}}]
  (when-let [ent  (d/q `[:find ?ent .
                         :where [?ent :schema/name ~schema-urn]] @conn)]
    (cond-> (dp/pull @conn '[*] ent)
      resolve? (du/resolve-db-id conn filter-set))))

(def diag-path "Pathname of error-producing file; for debugging; keep." (atom nil))

(defn add-schema-file!
  [path]
  (reset! diag-path path)
  (let [db-content (read-schema-file path)]
    (try
      (if (du/storable? db-content)
        (try (d/transact conn db-content) ; Use d/transact here, not transact! which uses a future.
             (catch Exception e
               (swap! bad-file-on-rebuild? conj path)
               (log/error "Error adding" path ":" e)))
        (do (swap! bad-file-on-rebuild? conj path)
            (reset! diag db-content)
            (log/error "Schema-map contains nils and cannot be stored." path)))
      (catch Exception e
        (swap! bad-file-on-rebuild? conj path)
        (log/error "Error checking storable?" path ":" e)))))

(defn add-schema-files!
  "Read a directory of files into the database.
   They consist of a rewritten (see rewrite-xsd) maps with some useful metadata.
    DIR is the directory to read from. All the .xsd files there will be added to the DB."
  [dir]
  (if (-> dir clojure.java.io/file .isDirectory)
    (let [grammar-matcher (.getPathMatcher
                           (java.nio.file.FileSystems/getDefault)
                           "glob:*.xsd")
          files (->> (file-seq (io/file dir))
                     (filter #(.isFile %))
                     (filter #(.matches grammar-matcher (.getFileName (.toPath %))))
                     (mapv #(.getAbsolutePath %)))]
      (doseq [file files]
        (add-schema-file! file)))
    (throw (ex-info "Directory does not exist:" {:dir dir}))))

(defn update-bad-files!
  "On rebuild, note what couldn't be read."
  []
  (when (:rebuild-db? db-cfg)
    (doseq [file @bad-file-on-rebuild?]
      (d/transact conn [{:schema/pathname file
                         :mm/fileNotRead? true}]))))

(defn bad-files
  "This creates a 'to do' list for debugging!"
  []
  (d/q '[:find [?p ...] :where
         [?ent :mm/fileNotRead? true]
         [?ent :schema/pathname ?p]]
       @conn))

(defn unused-attrs
  "Return a list of unused database attributes (for debugging)."
  []
  (let [unused (atom [])]
    (doseq [attr (map :db/ident db-schema)]
      (when (empty? (d/q `[:find ?e :where [?e ~attr ?]] @conn))
        (swap! unused conj attr)))
    @unused))

;;; =================================== Schema-db post-processing ==============================
(defn add-topics! []
  (let [forms (reduce (fn [forms urn]
                        (if-let [topic (schema-topic urn)]
                          (conj forms {:schema/name urn :schema/topic topic})
                          forms))
                      []
                      (list-schemas))]
    (d/transact conn forms)))

(defn fix-includes! ; Just used on OAGIS schema, UBL-CommonExtensionComponents-2.3.xsd AFAIK.
  "Files have :mm/tempInclude which are paths (typically relative)
   Add :schema/includedSchemas where these are resolved to references to :schema/name."
  []
  (let [temps (d/q '[:find ?ent ?i ?p :keys s/ent s/include-file s/s-path :where
                     [?ent :mm/tempInclude ?i]
                     [?ent :schema/pathname ?p]]
                   @conn)
        with-urn (map (fn [temp]
                        (let [[_ up _ ipath] (re-matches #"^((\.\./)*)(.*)$" (:s/include-file temp))
                              schema-path (clojure.java.io/file (:s/s-path temp))
                              up-cnt (inc (int (/ (count up) 3)))
                              dir    (du/dir-up schema-path up-cnt)
                              file   (str dir "/" ipath)]
                          (if-let [urn (d/q `[:find ?urn . :where
                                              [?e :schema/pathname ~file]
                                              [?e :schema/name ?urn]]
                                            @conn)]
                            (assoc temp :s/include urn)
                            (do (log/warn "While resolving includes, cannot find schema with :schema/pathname" file)
                                temp))))
                      temps)]
    (d/transact conn
                (mapv #(-> {}
                           (assoc :db/id (:s/ent %))
                           (assoc :schema/includedSchemas (:s/include %)))
                      (filter #(contains? % :s/include) with-urn)))))

(defn postprocess-schemas!
  "Do some additional work on schema already in the DB."
  []
  (add-topics!)
  (fix-includes!)
  (update-bad-files!))

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
                          [?s :schema/inlinedTypedefs ?ent]
                          [?ent :sp/type ~type-term]]
                      @conn)]
    (-> (du/resolve-db-id {:db/id ent} conn)
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
                     @conn)]
        (when (and ent lib)
          (-> (du/resolve-db-id {:db/id ent} conn)
              (assoc :mm/lib-where-found lib)
              (assoc :mm/access-method :imported-typedef)))))))

(defn model-sequence-type-ref
  [type-term schema-urn]
  (when-let [ent (d/q `[:find ?m . :where
                        [?s  :schema/name ~schema-urn]
                        [?s  :model/sequence ?m]
                        [?m  :sp/type ~type-term]]
                      @conn)]
    (-> (du/resolve-db-id {:db/id ent} conn)
        (assoc :mm/access-method :model-sequence-type))))

#_(defn model-sequence-name-ref
  "Return the object that has term somewhere in its :model/sequence."
  [term schema-urn]
  (when-let [ent (d/q `[:find ?m . :where
                        [?s  :schema/name ~schema-urn]
                        [?s  :model/sequence ?m]
                        [?m  :sp/name ~term]]
                      @conn)]
    (let [found (du/resolve-db-id {:db/id ent} conn)]
      (-> {}
          (assoc :mm/access-method :model-sequence-name)
          (assoc :model/sequence (:model/sequence found))))))

(defn schema-ref
  "The term is found in the :model/sequence of a schema; return the schema."
  [term schema-urn]
  (when-let [ent (d/q `[:find ?s . :where
                        [?s  :schema/name ~schema-urn]
                        [?s  :schema/type :ccts/message-schema]
                        [?s  :model/sequence ?m]
                        [?m  :sp/name ~term]
                        [?m  :sp/type ?type]]
                      @conn)]
    (let [found (du/resolve-db-id {:db/id ent} conn)]
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
                      @conn)]
    (-> (du/resolve-db-id {:db/id ent} conn)
        (assoc :mm/access-method :included-typedef))))

;;;(library-lookup-ref "UBLExtension" "urn:oasis:names:specification:ubl:schema:xsd:CommonExtensionComponents-2")
(defn library-lookup-ref
  "Find the term as :schema/content"
  [term schema-urn]
  (when-let [ent (d/q `[:find ?c . :where
                        [?s :schema/name ~schema-urn]
                        [?s :schema/content ?c]
                        [?c :sp/name ~term]]
                      @conn)]
    (-> (du/resolve-db-id {:db/id ent} conn)
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
(defn store-test [] (->> "small-invoice-schema-1.edn" read-string vector (d/transact conn)))
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
                          @conn) ; ToDo: Datahike OR an NOT queries not implemented??? Use predicate?
                     (d/q `[:find ?content .
                            :where
                            [?schema  :schema/name ~schema-urn]
                            [?schema  :schema/content ?content]
                            [?content :term/type ~term]
                            [?content :sp/function ?fn]
                            [?fn      :fn/componentType :BBIE]]
                          @conn))]
    (du/resolve-db-id (dp/pull @conn '[*] ent) conn)))

;;; (get-term "urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2" "InvoiceLine")
;;; (get-term "urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2" "DeliveryTerms")
;;; (get-term "urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2" "BestBeforeDate")
;;; I seem to need something different for 'types' e.g. "InvoiceLineType"
(defn get-term
  "Return a map describing what is known about the argument data type.
    - Schema-urn is a string
    - term is a string naming a schema object such as a data type (e.g. 'AmountType')"
  [schema-urn term & {:keys [expand-type?] :or {expand-type? true}}]
  (when-let [ent (d/q `[:find ?content .
                        :where
                        [?schema :schema/name ~schema-urn]
                        [?schema :schema/content ?content]
                        [?content :term/name ~term]]
                      @conn)]
    (let [not-typed (du/resolve-db-id (dp/pull @conn '[*] ent) conn)]
      (if expand-type?
        (if-let [type (:term/type not-typed)]
          (if-let [type-info (get-term-type schema-urn type)]
            (assoc not-typed :type-info type-info)
            not-typed)
          not-typed)
        not-typed))))

(defn get-term-schemas
  "Return all the schema containing the term (currently schema/term)"
  [term]
  (when-let [ents (not-empty (d/q `[:find [?ent ...]
                                    :where [?ent :term/name ~term]]
                                  @conn))]
    (mapv (fn [ent] (d/q `[:find ?s .
                           :where
                           [?ent :schema/content ~ent]
                           [?ent :schema/name ?s]]
                         @conn))
          ents)))

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
      (du/resolve-db-id ?s conn))))

(defn mf-meth
  "Get Michael's TestMethod Schema"
  []
  (query-for [{[:schema/name "urn:oagis-10.6:TestMethod-MF"] [:sdb/schema-id]}]))

;;;================================ Starting and Stopping ===========================================
;;; (user/restart) whenever you update the DB or the resolvers. (tools/refresh) if compilation fails.

(defn create-db!
  "Create the database if :rebuild? is true, otherwise just set the connection atom, conn."
  []
  (util/config-log :info)
  (when (:rebuild-db? db-cfg)
    (reset! bad-file-on-rebuild? #{})
    (when (d/database-exists? db-cfg) (d/delete-database db-cfg))
    (d/create-database db-cfg)
    (alter-var-root (var conn) (fn [_] (d/connect db-cfg)))
    (d/transact conn db-schema)
    (add-schema-files! (str ubl-root "maindoc"))
    (add-schema-files! (str ubl-root "common"))
    (add-schema-files! (str oagis-10-8-root "Nouns"))
    (add-schema-files! (str oagis-10-8-root "Platform/2_7/Common"))
    (add-schema-files! (str qif-root "QIFApplications"))
    (add-schema-files! (str qif-root "QIFLibrary"))
    ;(add-schema-files! michael-root) ; Currently has nils
    (postprocess-schemas!)
    (log/info "Created schema DB")))

(defn get-db-atm
  "Do a d/connect to the database, returning a connection atom."
  []
  (if (d/database-exists? db-cfg)
    (d/connect db-cfg)
    (log/warn "There is no example DB to connect to.")))

(defn connect-db
  "Set the var rad-mapper.schema-db/conn by doing a d/connect."
  []
  (if (d/database-exists? db-cfg)
    (alter-var-root (var conn) (fn [_] (d/connect db-cfg))),
    (log/warn "There is no DB to connect to.")))

(defstate schema
  :start
  (do
    (util/config-log :info)
    (connect-db)))
