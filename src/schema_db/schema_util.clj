(ns schema-db.schema-util
  "Functions to classify schema, include standard messaging schema."
  (:require
   [clojure.pprint               :refer [cl-format]]
   [clojure.set                  :refer [intersection]]
   [clojure.string               :as str]
   [datahike.api                 :as d]
   [datahike.pull-api            :as dp]
   [schema-db.db-util :as du     :refer [connect-atm xpath xml-type?]]
   [schema-db.schema  :as schema :refer [db-schema-rekey simple-xsd? generic-schema-type? special-schema-type? cct-tag2db-ident-map]]
   [taoensso.timbre              :as log]))

(def ^:dynamic *skip-doc-processing?*
  "If true, it won't go through (bug-ridden) obj-doc-string"
  true)

(def ^:dynamic *prefix* "String naming a namespace for attributes whose name appears in lots of objects (e.g. model, element, complexType, simpleType)"
  "unassigned")

(defn ignored-tag?
  "Return true if the tag is to be ignored."
  [obj]
  (when-let [tag (:xml/tag obj)]
    (and (= "ROOT" (namespace tag))
         (= "srt_" (-> tag name (subs 0 4))))))

;;; UBL-xmldsig-core-schema-2.3.xsd:
;;; <simpleType name="CryptoBinary">
;;;  <restriction base="base64Binary">
;;;  </restriction>
;;; </simpleType>
;;; is example of one with string crap content [\newline \space \space].
;;; I think I need this to be able to return string content, so I'm going
;;; to address this in the caller, gs/extend-restrict. ToDo: Not investigated.
(defn xml-group-by
  "Return a map of the :xml/content of the argument grouped by the keys (without the ns) and
   :other if the :xml/tag is not one of the provided keys."
  [xmap & tag-keys]
  (let [tkey? (set tag-keys)]
    (cond-> (group-by
             #(if-let [k (tkey? (:xml/tag %))] (-> k name keyword) :other)
             (:xml/content xmap))
      (:xml/attrs xmap) (assoc :attrs (:xml/attrs xmap)))))

;;; This does file-level dispatching as well as the details.
;;; The naming rules:
;;;        - schema_type will always be a _ keyword.
;;;        - file-level dispatch uses :qualified/camelCase.
;;;        - lower-level dispatch uses :qualified/kebab-case.
(defn rewrite-xsd-dispatch
  [obj & [specified]]
   (let [stype (:schema_type obj)
         schema-sdo  (:schema_sdo obj)]
     (cond ;; Optional 2nd argument specifies method to call
       (keyword? specified) specified,

       ;; Files (schema-type) use :qualified/CamelCase
       (and (= stype :ccts_messageSchema)    (= schema-sdo :oasis))  :std/messageSchema,
       (and (= stype :ccts_messageSchema)    (= schema-sdo :oagi))   :std/messageSchema,
       (and (= stype :ccts_componentSchema)  (= schema-sdo :oagi))   :generic/qualifiedDtypeSchema,
       (and (= stype :ccts_componentSchema)  (= schema-sdo :oasis))  :oasis/componentSchema,
       (and (= stype :generic_messageSchema) (= schema-sdo :qif))    :generic/xsdFile,
       (and (= stype :niem_domainSchema)     (= schema-sdo :niem))   :generic/qualifiedDtypeSchema, ; ToDo: review this choice
       (and (= stype :niem_codeListSchema)   (= schema-sdo :niem))   :niem/codeListSchema, ; ToDo: review this choice
       (= stype :generic_qualifiedDtypeSchema)                       :generic/qualifiedDtypeSchema
       (= stype :generic_unqualifiedDtypeSchema)                     :generic/unqualifiedDtypeSchema
       (= stype :generic_xsdFile)                                    :generic/xsdFile
       (= stype :generic_codeListSchema)                             :generic/codeListSchema
       (= stype :generic_librarySchema)                              :generic/librarySchema

       (special-schema-type? stype) stype,
       (generic-schema-type? stype) stype,

       ;; Special simplifications use :qualified/kebab-case
       (and (map? obj) (contains? simple-xsd?           (:xml/tag obj)))  :generic/simple-xsd-elem,
       (and (map? obj) (contains? cct-tag2db-ident-map  (:xml/tag obj)))  :generic/simple-cct,
       (and (map? obj) (ignored-tag? obj))                                :ignore/ignore,
       (and (map? obj) (= "ccts" (-> obj :xml/tag namespace)))            :oasis/component-def,
       (and (map? obj) (contains? obj :xml/tag))                          (:xml/tag obj),
       (and (map? obj) (contains? obj :ref))                              :ref
                                        ; ToDo: this one for polymorphism #{:xsd*/element :xsd*/choice} etc.
       (and (map? obj) (contains? obj :xml/tag))                           (:xml/tag obj)
       :else (throw (ex-info "No method for obj: " {:obj obj})))))


;;; This is bug-ridden and should be avoided!
(defn obj-doc-string
  "If the object has annotations in its :xml/content, remove them and return
   modified object and the string content. Otherwise, just return object and nil.
   The xsd*:appinfo optional content is ignored."
  [obj]
  (log/warn "Using obj-doc-string.")
  (if (or *skip-doc-processing?*
          #_(-> obj :xml/content string?) ; 2023-03-21 I commented this out and put the other two back!
          (not (map? obj)) ; ToDo: This and next are uninvestigated problems in the etsi files.
          (not (contains? obj :xml/content))) ; 2023-03-17 I commented those out and added string? test.
    [obj nil]
    (let [parts (group-by #(xml-type? % :xsd*/annotation) (:xml/content obj))
          annotes (get parts true)
          obj-annote-free (assoc obj :xml/content (vec (get parts false)))
          s (when (not-empty annotes)
              (str/trim
               (reduce (fn [st a]
                         (str st "\n" (let [docs (filter #(xml-type? % :xsd*/documentation) (:xml/content a))]
                                        (reduce (fn [more-s d]
                                                  (if (-> d :xml/content string?)
                                                    (str more-s "\n" (str/trim (:xml/content d)))
                                                    more-s))
                                                ""
                                                docs))))
                       ""
                       annotes)))]
      [obj-annote-free (if (and (string? s) (re-matches #"^\s*$" s)) nil s)]))) ; ToDo: expensive?


(defn q-schema-topic
  "Lookup the topic for a schema in the DB."
  [urn]
  (d/q `[:find ?topic .
         :where [?s :schema_name ~urn]
         [?s :schema_topic ?topic]] @(connect-atm)))

(defn q-schema-sdo
  "Lookup the SDO for a schema in the DB."
  [urn]
  (d/q `[:find ?sdo .
         :where [?s :schema_name ~urn]
         [?s :schema_sdo ?sdo]] @(connect-atm)))

(defn q-schema-type
  "Lookup the type for a schema in the DB."
  [urn]
  (d/q `[:find ?type .
         :where [?s :schema_name ~urn]
         [?s :schema_type ?type]] @(connect-atm)))

(defn schema-ns
  "Return the namespace urn string for the argument xmap."
  [xmap]
  (or (-> (xpath xmap :xsd*/schema)  :xml/attrs :targetNamespace)
      (-> (xpath xmap :ROOT/schema) :xml/attrs :targetNamespace))) ; for some UBL-provided w3c schema

(defn schema-sdo
  "Return a keyword identifying the XML file's standards development organization."
  [xmap]
  (if-let [ns (schema-ns xmap)]
    (cond ;(re-matches #"^urn:oasis:[\w,\-,\:]+:ubl:[\w,\-,\:]+$" ns) :oasis,
          (re-matches #"^urn:oasis:[\w,\-,\:].*$" ns) :oasis,
          (re-matches #"^urn:un:unece:uncefact:[\w,\-,\:]+$" ns) :cefact, ; cefact via UBL ; ToDo: NONE OF THESE???
          (re-matches #"^http://www.openapplications.org/oagis/.*$" ns) :oagi,
          (re-matches #"^http://release.niem.gov/niem/.*$" ns) :niem
          (re-matches #"^http://reference.niem.gov/niem/.*$" ns) :niem
          (re-matches #"^urn:iso:std:iso:20022:tech:xsd:pain.+$" ns) :iso ; ISO via oagis
          (re-matches #"^http://uri.etsi.*" ns) :etsi
          (re-matches #"^http://www.w3.org/.*" ns) :w3c
          (re-matches #"^http://qifstandards.org/xsd/qif3" ns) :qif)
    (do (log/warn "Cannot determine schema SDO:" (:schema_pathname xmap) " using :unknown.")
        :unknown)))

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
   This is not necessarily unique! schema-topic is called after most processing is done.
   Thus it can be called with just the db/unique :schema_name."
  [xmap]
  (let [desc [(:schema_sdo xmap) (:schema_type xmap)]
        name (:schema_name xmap)
        res (cond (= desc [:oasis :ccts_messageSchema])
                  (->> name (re-matches #"^urn:oasis:names:specification:ubl:schema:xsd:(.+)-\d$") second),

                  (= desc [:oagi :ccts_messageSchema])
                  ;; urn:oagis-10.8.4:Nouns:ProjectAccounting
                  (or (->> name (re-matches #"^urn:oagis-[\d,\.]+:Nouns:(.+)$") second),
                      (->> name (re-matches #"^urn:oagis-[\d,\.]+:Components:(.+)$") second)),

                  (= desc [:oagi :generic_codeListSchema])
                  (->> name (re-matches #"^urn:oagis-\d+\.\d+:(.+)$") second),

                  (= desc [:qif :generic_messageSchema])
                  (->> name (re-matches #"^urn:QIF-.*:Application:QIF(.+)$") second),

                  (= desc [:qif :generic_librarySchema])
                  (->> name (re-matches #"^urn:QIF-.*:Library:(.+)$") second),

                  (= desc [:niem :niem_domainSchema])
                  (->> name (re-matches #"^urn:niem.*Domain:(.+)$") second),

                  (= desc [:niem :niem_codeListSchema])
                  "CodeList"

                  (contains? non-standard-schema-topics name)
                  (get non-standard-schema-topics name))]
    (or res
        (do (log/warn "Cannot determine schema topic: name = " name "file ="  (:schema_pathname xmap))
            "unknown"))))

(defn schema-spec
  "Return a keyword signifying the specification of which the schema is part.
   These are #{:ubl, :oagis, etc.}. It is used as :schema/spec"
  [xmap]
  (let [ns   (schema-ns xmap)
        spec (case (:schema_sdo xmap)
               :cefact     (when (= ns "urn:un:unece:uncefact:data:specification:CoreComponentTypeSchemaModule:2") :cefact-ccl),
               :oasis      (cond (re-matches #"^urn:oasis:[\w,\-,\:]+:ubl:[\w,\-,\:]+(\-2)$" ns)                   :ubl
                                 (= ns "urn:oasis:names:specification:bdndr:schema:xsd:UnqualifiedDataTypes-1")    :ubl) ; I hesitate to call it :oasis
               :oagi       (when (re-matches #"^http://www.openapplications.org/oagis/10$" ns)                     :oagis)
               :niem       :niem
               :iso        :iso-20022
               :etsi       :etsi-1903 ; ToDo: guessing
               :w3c        :w3c       ; In QIF somewhere!
               :qif        :qif
               nil)]
    (or spec (do (log/warn "Cannot determine schema-spec:" (:schema_pathname xmap) " Using :default.")
                 :unknown))))

(defn schema-version
  [xmap]
  (case (:schema_spec xmap)
    :ubl    "2"
    :oagis  "10"
    :qif    (let [[_ n] (re-matches #"^http://qifstandards.org/xsd/qif(\d)" (schema-ns xmap))]
              (or n ""))
    :niem   (let [[_ n] (or (re-matches #"^http://release.niem.gov/niem/.+/([0-9])\.[0-9]/.+$" (schema-ns xmap))
                            (re-matches #"^http://reference.niem.gov/niem/.+/([0-9])\.[0-9]/.+$" (schema-ns xmap)))]
              (or n ""))
    "Ver[unknown]"))

(defn schema-subversion
  "Return the subversion. NB: Currently this is hard coded. Should be an environment variable." ; ToDo: need env.
  [xmap]
  (let [spec   (:schema_spec xmap)
        pname  (:schema_pathname xmap)]
    (cond (= spec :ubl)          "3"
          (= spec :oagis)        (let [[_ subver] (re-matches #".*OAGIS/[0-9]+\.([0-9,\.]+).*" pname)]
                                   (or subver
                                       (log/warn "Could not determine OAGIS subversion.")
                                       ""))
          (= spec :cefact-ccl)   "" ; ToDo: See also UBL CCL.
          (= spec :niem)         (let [[_ n] (or (re-matches #"^http://release.niem.gov/niem/.+/[0-9]\.([0-9])/.+$" (schema-ns xmap))
                                                 (re-matches #"^http://reference.niem.gov/niem/.+/[0-9]\.([0-9])/.+$" (schema-ns xmap)))]
                                   (or n ""))
          :else                  "")))

;;; ToDo: Make this a multi-method (on the case values)
(defn schema-type
  "Return a keyword signifying the specification of which the schema is part.
   These are #{:ubl-2, :oagis-10, etc.}. It is used as :schema_spec
   This is just a first guess; see schema/update-schema-type."
  [xmap]
  (let [ns (schema-ns xmap)
        sdo (:schema_sdo xmap)
        pname (:schema_pathname xmap)]
    (case sdo
      :cefact
      (cond (= ns "urn:un:unece:uncefact:data:specification:CoreComponentTypeSchemaModule:2")
            :generic_unqualifiedDtypeSchema
            :else (log/warn "Cannot determine schema-type:" pname))

      :oasis
      (cond (= ns "urn:oasis:names:specification:ubl:schema:xsd:UnqualifiedDataTypes-2")
            :generic_unqualifiedDtypeSchema,
            (= ns "urn:oasis:names:specification:bdndr:schema:xsd:UnqualifiedDataTypes-1")
            :generic_unqualifiedDtypeSchema,
            (= ns "urn:oasis:names:specification:ubl:schema:xsd:QualifiedDataTypes-2")
            :generic_qualifiedDtypeSchema,
            (re-matches #"^urn:oasis:names:specification:ubl:schema:xsd:Common[\w,\-,\:]+Components\-2$" ns)
            :ccts_componentSchema
            (re-matches #"^urn:oasis:names:specification:ubl:schema:xsd:\w+-2$" ns)
            :ccts_messageSchema
            :else (log/warn "Cannot determine schema-type:" pname))

      :oagi ; no URNs; guess based on pathname.
      (cond (re-matches #".*Fields\.xsd$" pname) ; though it is in the "Components" directory
            :generic_unqualifiedDtypeSchema
            (re-matches #".+CodeLists.+" pname)
            :generic_codeListSchema
            (re-matches #".+Components.+" pname)
            :ccts_componentSchema
            (re-matches #"^http://www.openapplications.org/oagis/10$" ns)
            :ccts_messageSchema ; ToDo: Not quite correct.
            :else (do (log/warn "Cannot determine schema-type:" pname)
                      :generic_xsdFile))

      :niem
      (cond (re-matches #"^http://release.niem.gov/niem/codes/.*" ns)
            :niem_codeListSchema ; This is new, just for NIEM so far! See, for example, codes/nga.xsd.
            (re-matches #"^http://release.niem.gov/niem/domains/.*" ns)
            :niem_domainSchema  ; This is new, just for NIEM so far! See, for example, domains/agriculture.xsd. Treat it like :generic_libaray-schema
            (re-matches #"http://release.niem.gov/niem/niem-core/.*" ns)
            :generic_librarySchema ; The NIEM use of this niem-core.xsd, seems to be a mixed bag of types.
            :else (do (log/warn "Cannot determine schema-type:" pname)
                      :generic_xsdFile))

      :etsi :generic_xsdFile

      :iso :iso_iso20022Schema

      :qif
      (cond (re-matches #".*QIFApplications/.*.xsd$" pname) :generic_messageSchema
            (re-matches #".*QIFLibrary/.*.xsd$"   pname)    :generic_librarySchema
            :else (do (log/warn "Cannot determine schema-type:" pname)
                      :generic_xsdFile))
      ;; Default
      :generic_xsdFile)))

(declare schema-name-special schema-name-std)
(defn schema-name
  "Return the name of the schema object. This uses the XML content to determine one."
  [xmap]
  (let [
        pname (:schema_pathname xmap)]
    (if (re-matches #".*sources/misc/.*" pname)
      (schema-name-special xmap)
      (schema-name-std xmap))))

(defn schema-name-special
  [xmap]
  (let [sdo  (:schema_sdo xmap)
        ver  (:schema_version xmap)
        sver (:schema_subversion xmap)
        pname (:schema_pathname xmap)]
    (if-let [[_ person dir fname] (re-matches #".*sources/misc/([a-zA-Z0-9\-\_]+)/([a-zA-Z0-9\-\_]+)/([a-zA-Z0-9\-\_]+).xsd" pname)]
      (cl-format nil "urn:~A-~A.~A:~A.~A.~A" (name sdo) ver sver person dir fname)
      (do (log/warn "Could not determine special schema name.")
           "unknown/special"))))

(defn schema-name-std
  [xmap]
  (let [sdo  (:schema_sdo xmap)
        ver  (:schema_version xmap)
        sver (:schema_subversion xmap)
        ver-str (if (empty? sver) ver (str ver "." sver ))
        pname (:schema_pathname xmap)
        res-pname (-> pname (str/split #"/") last (str/split #"\.") first)]
    (cond
      (= :oagi sdo)
      (if-let [[_ fname] (re-matches #".*Components/(\w+).xsd" pname)]
        (str "urn:oagis-" ver-str ":Components:" fname)
        (if-let [[_ fname] (re-matches #".*Model/Nouns/(\w+).xsd" pname)]
          (str "urn:oagis-" ver-str ":Nouns:" fname)
          (if-let [[_ fname] (re-matches #".*Common/ISO20022/(pain[0-9,\.]+).xsd" pname)]
            (str "urn:oagis-" ver-str ":Common:" fname)
            (if-let [name  (-> (xpath xmap :xsd*/schema :xsd*/element) :xml/attrs :name)]
              (str  "urn:oagis-" ver-str ":" name)
              (if res-pname
                (do (log/warn "Using pathname to define OAGIS" ver-str "schema name:" res-pname)
                    (str "urn:oagis-" ver-str ":" res-pname))
                (do (log/warn "Could not determine OAGIS" ver-str "schema name:" pname)
                    :mm_nil)))))),
      (= :niem sdo)
      (if-let [[_ fname] (re-matches #".*codes/(\w+).xsd" pname)]
        (str "urn:niem-" ver-str ":Code:" fname)
        (if-let [[_ fname] (re-matches #".*domains/(\w+).xsd" pname)]
          (str "urn:niem-" ver-str ":Domain:" fname)
          (if (re-matches #".*niem-core.xsd" pname)
            (str "urn:niem-" ver-str ":Core:core-schema")
            (if res-pname
              (do (log/warn "Using pathname to define NIEM" ver-str "schema name:" res-pname)
                  (str "urn:niem-" ver-str ":" res-pname))
              (do (log/warn "Could not determine NIEM" ver-str "schema name:" pname)
                  :mm_nil))))),

      (= :qif sdo)
      (if-let [[_ fname] (re-matches #".*/QIFLibrary/(\w+).xsd" pname)]
        (str "urn:QIF-" ver-str ":Library:" fname)
        (if-let [[_ fname] (re-matches #".*/QIFApplications/(\w+).xsd" pname)]
          (str "urn:QIF-" ver-str ":Application:" fname)
          (do (log/warn "Could not determine QIF" ver-str "schema name.") :mm_nil)))

      (#{:oasis :cefact :iso :etsi} sdo)
      (if-let [name (schema-ns xmap)]
        name
        (do (log/warn "Could not determine UBL or ISO schema name:" pname) ""))
      :else
      (let [fname (-> pname (str/split #"/") last)]
        (log/warn "Could not determine schema name:" pname " Using " fname)
        fname))))

(defn update-schema-type
  "After you've parsed the whole schema, its type is much more apparent.
   Specifically, what was :ccts_message-schema might be be a :ccts_bie."
  [schema]
  (let [bie? (atom false)]
    (letfn [(b? [obj]
              (cond @bie? true
                    (map? obj)    (doseq [[k v] (seq obj)]
                                    (when (= k :cct_BusinessContext)
                                      (reset! bie? true))
                                    (b? v))
                    (vector? obj) (doall (map b? obj))))]
      (b? schema)
      ;(log/info "Final :schema_type changed?: " @bie?)
      (cond-> schema
        @bie? (assoc :schema_type :cct_bie)))))

(defn singleize
  [obj]
  (if (== 1 (count obj)) (first obj) obj))

;;; Where this is used I'm typically trying to finesse the output into somethign less
;;; clunky than the shape of XSD/XML. ToDo: Still experimental!
(defn merge-warn
  "Merge the argument maps but warn where any of them have the same keys (collisions),
   unless the keys are are on the ok-set. Values of collision keys are aggregated."
  [maps & {:keys [ok-set] :or {ok-set #{:has_docString :has_documentation}}}]
  (if (== 1 (count maps))
    maps
    (let [collisions (->> maps (map keys) (map set) (apply intersection))
          save-data (reduce (fn [res k]
                              (assoc res k (reduce (fn [gather m]
                                                     (if (contains? m k)
                                                       (let [more (get m k)]
                                                         (if (vector? more)
                                                           (into gather more)
                                                           (conj gather more)))
                                                       gather))
                                                   []
                                                   maps)))
                            {}
                            collisions)]
      (when (-> collisions set (clojure.set/difference ok-set) not-empty)
        (log/warn "Maps have same keys: " collisions "\n save-data =" save-data))
      (reduce-kv (fn [m k v] (assoc m k v))
                 (apply merge maps)
                 save-data))))

(defn xsd-value-type
  "Arguments are a db-ident and value.
   Returns the value coerced to that type."
  [k v]
  (if-let [typ (-> db-schema-rekey k :db/valueType)]
    (try
      (if (#{:db.type/long :db.type/float :db.type/number, :db.type/double :db.type/boolean} typ)
        (let [val (read-string v)]
          (if (= val 'unbounded) -1 val))
        v)
      (catch Exception _e (log/warn "Could not read xsd attribute declared a number: k = " k " v = " v)))
      (log/warn "Unknown attribute type: " k)))

;;; ToDo: All of the things currently in ns "xsd*" should be in a NS called "xsdAttr".
(defn xsd-attr-map
  "Return a map of attributes where
    (1) the keys of a few special ones are in the argument namespace (a string) and the rest are in 'xsd', and_
    (2) the values are converted to the right type, based on the database schema."
  [attr-map nspace]
  (reduce-kv
   (fn [m k v]
     (let [kk (if (#{:ref :name :id} k)
                (keyword (str nspace "_" (name k)))
                (keyword (str "xsd" "_" (name k))))] ; 4th
       (assoc m kk (xsd-value-type kk v))))
   {}
   attr-map))

;;; ToDo: This isn't used yet. Might not be needed.
#_(defn create-lookup-refs
  "Return a vector of datahike:    {:db/add -1 attr val}
                      datascript:  {:db/id   n attr val}, where n={1,2,3...}
   corresponding to entities to establish before sending data.
   From datahike api.cljc:
                      ;; create a new entity ('-1', as any other negative value, is a tempid
                      ;; that will be replaced by Datahike with the next unused eid)
                      (transact conn [[:db/add -1 :name \"Ivan\"]])"
  [schema data cljs?]
  (let [db (if cljs? :datascript :datahike)
        lookup-refs (atom #{})
        cnt (atom 0)
        ref-ident? (reduce-kv (fn [r k v] (if (contains? v :db/unique) (conj r k) r)) #{} schema)]
    (letfn [(dlr [obj]
              (cond (map? obj)    (doseq [[k v] (seq obj)] ; v should already be a string. ToDo: It isn't!
                                    (when (and (ref-ident? k) (not (known-lookup? @lookup-refs k v db)))
                                        (swap! lookup-refs conj (case db
                                                                  :datascript {:db/id (swap! cnt inc) k v}
                                                                  :datahike   [:db/add -1 k v])))
                                    (dlr v))
                    (vector? obj) (doall (map dlr obj))))]
      (dlr data)
      (-> @lookup-refs vec))))


;;;=========================== Schema Operations ===========================================
;;; (list-schemas :sdo :oagi)
(defn list-schemas
  "Return a list of schema, by default they are sorted.
   Attrs should be a list of attributes to return"
  [& {:keys [sdo sort? _attrs] :or {sort? true sdo '_}}] ; ToDo: attrs
  (let [result (d/q '[:find ?n ?sdo
                      :keys schema_name schema_sdo
                      :in $ ?sdo
                      :where
                      [?s :schema_name ?n]
                      [?s :schema_sdo ?sdo]]
                    @(connect-atm) sdo)]
    (cond->> result
      true (map :schema_name)
      sort? sort
      true vec)))

;;; (get-schema "urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_1")
(defn get-schema
  "Return the map stored in the database for the given schema-urn. Useful in development.
    :filter-set - DB attribute to leave out (e.g. #{:db/id} or #{:db/doc-string}) "
  [schema-name & {:keys [resolve? filter-set] :or {resolve? true filter-set #{:db/id}}}]
  (when-let [ent  (d/q '[:find ?ent .
                         :in $ ?schema-name
                         :where [?ent :schema_name ?schema-name]]
                       @(connect-atm) schema-name)]
    (cond-> (dp/pull @(connect-atm) '[*] ent)
      resolve? (du/resolve-db-id (connect-atm) filter-set))))
