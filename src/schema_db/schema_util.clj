(ns schema-db.schema-util
  "Functions to classify schema, include standard messaging schema."
  (:require
   [clojure.pprint               :refer [cl-format]]
   [clojure.set                  :refer [intersection]]
   [clojure.string               :as str]
   [datahike.api                 :as d]
   [datahike.pull-api            :as dp]
   [schema-db.db-util :as du     :refer [connect-atm xpath xml-type?]]
   [schema-db.schema  :as schema :refer [db-schema+ simple-xsd? generic-schema-type? special-schema-type? cct-tag2db-ident-map]]
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
(defn rewrite-xsd-dispatch
  [obj & [specified]]
   (let [stype (:schema/type obj)
         schema-sdo  (:schema/sdo obj)]
     (cond ;; Optional 2nd argument specifies method to call
       (keyword? specified) specified,
       
       ;; Files (schema-type)
       (and (= stype :ccts/message-schema)    (= schema-sdo :oasis))  :std/message-schema, 
       (and (= stype :ccts/message-schema)    (= schema-sdo :oagi))   :std/message-schema,
       (and (= stype :ccts/component-schema)  (= schema-sdo :oagi))   :generic/qualified-dtype-schema,
       (and (= stype :ccts/component-schema)  (= schema-sdo :oasis))  :oasis/component-schema,
       (and (= stype :generic/message-schema) (= schema-sdo :qif))    :generic/xsd-file,
       
       (special-schema-type? stype) stype,
       (generic-schema-type? stype) stype,
       
       ;; Special simplifications
       (and (map? obj) (contains? simple-xsd?           (:xml/tag obj)))  :generic/simple-xsd-elem,
       (and (map? obj) (contains? cct-tag2db-ident-map  (:xml/tag obj)))  :generic/simple-cct,
       (and (map? obj) (ignored-tag? obj))                                :ignore/ignore,
       (and (map? obj) (= "ccts" (-> obj :xml/tag namespace)))            :oasis/component-def,
       (and (map? obj) (contains? obj :xml/tag))                          (:xml/tag obj),
       (and (map? obj) (contains? obj :ref))                              :ref
                                        ; ToDo: this one for polymorphism #{:xsd/element :xsd/choice} etc.
       (and (map? obj) (contains? obj :xml/tag))                           (:xml/tag obj)
       :else (throw (ex-info "No method for obj: " {:obj obj})))))


;;; This is bug-ridden and should be avoided!
(defn obj-doc-string
  "If the object has annotations in its :xml/content, remove them and return
   modified object and the string content. Otherwise, just return object and nil.
   The xsd:appinfo optional content is ignored."
  [obj]
  (log/warn "Using obj-doc-string.")
  (if (or *skip-doc-processing?*
          #_(-> obj :xml/content string?) ; 2023-03-21 I commented this out and put the other two back!
          (not (map? obj)) ; ToDo: This and next are uninvestigated problems in the etsi files.
          (not (contains? obj :xml/content))) ; 2023-03-17 I commented those out and added string? test.
    [obj nil]
    (let [parts (group-by #(xml-type? % :xsd/annotation) (:xml/content obj))
          annotes (get parts true)
          obj-annote-free (assoc obj :xml/content (vec (get parts false)))
          s (when (not-empty annotes)
              (str/trim
               (reduce (fn [st a]
                         (str st "\n" (let [docs (filter #(xml-type? % :xsd/documentation) (:xml/content a))]
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
         :where [?s :schema/name ~urn]
         [?s :schema/topic ?topic]] @(connect-atm)))

(defn q-schema-sdo
  "Lookup the SDO for a schema in the DB."
  [urn]
  (d/q `[:find ?sdo .
         :where [?s :schema/name ~urn]
         [?s :schema/sdo ?sdo]] @(connect-atm)))

(defn q-schema-type
  "Lookup the type for a schema in the DB."
  [urn]
  (d/q `[:find ?type .
         :where [?s :schema/name ~urn]
         [?s :schema/type ?type]] @(connect-atm)))

(defn schema-ns
  "Return the namespace urn string for the argument xmap."
  [xmap]
  (or (-> (xpath xmap :xsd/schema)  :xml/attrs :targetNamespace)
      (-> (xpath xmap :ROOT/schema) :xml/attrs :targetNamespace))) ; for some UBL-provided w3c schema

(defn schema-sdo
  "Return a keyword identifying the XML file's standards development organization."
  [xmap]
  (if-let [ns (schema-ns xmap)]
    (cond ;(re-matches #"^urn:oasis:[\w,\-,\:]+:ubl:[\w,\-,\:]+$" ns) :oasis,
          (re-matches #"^urn:oasis:[\w,\-,\:].*$" ns) :oasis,
          (re-matches #"^urn:un:unece:uncefact:[\w,\-,\:]+$" ns) :cefact, ; cefact via UBL ; ToDo: NONE OF THESE???
          (re-matches #"^http://www.openapplications.org/oagis/.*$" ns) :oagi,
          (re-matches #"^urn:iso:std:iso:20022:tech:xsd:pain.+$" ns) :iso ; ISO via oagis
          (re-matches #"^http://uri.etsi.*" ns) :etsi
          (re-matches #"^http://www.w3.org/.*" ns) :w3c
          (re-matches #"^http://qifstandards.org/xsd/qif3" ns) :qif)
    (do (log/warn "Cannot determine schema SDO:" (:schema/pathname xmap) " using :unknown.")
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

(defn schema-spec
  "Return a keyword signifying the specification of which the schema is part.
   These are #{:ubl, :oagis, etc.}. It is used as :schema/spec"
  [xmap]
  (let [ns   (schema-ns xmap)
        spec (case (:schema/sdo xmap)
               :cefact     (when (= ns "urn:un:unece:uncefact:data:specification:CoreComponentTypeSchemaModule:2") :cefact-ccl),
               :oasis      (cond (re-matches #"^urn:oasis:[\w,\-,\:]+:ubl:[\w,\-,\:]+(\-2)$" ns)                   :ubl
                                 (= ns "urn:oasis:names:specification:bdndr:schema:xsd:UnqualifiedDataTypes-1")    :ubl) ; I hesitate to call it :oasis
               :oagi       (when (re-matches #"^http://www.openapplications.org/oagis/10$" ns)                     :oagis)
               :iso        :iso-20022
               :etsi       :etsi-1903 ; ToDo: guessing
               :w3c        :w3c       ; In QIF somewhere!
               :qif        :qif
               "default")]
    (or spec (do (log/warn "Cannot determine schema-spec:" (:schema/pathname xmap) " Using :default.")
                 :unknown))))

(defn schema-version
  [xmap]
  (case (:schema/spec xmap)
    :ubl    "2"
    :oagis  "10"
    :qif    (let [[_ n] (re-matches #"^http://qifstandards.org/xsd/qif(\d)" (schema-ns xmap))]
              (or n ""))
    "unknown"))

(defn schema-subversion
  "Return the subversion. NB: Currently this is hard coded. Should be an environment variable." ; ToDo: need env.
  [xmap]
  (let [spec   (:schema/spec xmap)
        pname  (:schema/pathname xmap)]
    (cond (= spec :ubl) "3"
          (= spec :oagis)
          (let [[_ subver] (re-matches #".*OAGIS/[0-9]+\.([0-9,\.]+).*" pname)]
            (or subver
                (log/warn "Could not determine OAGIS subversion.")
                "unknown"))
          (= spec :cefact-ccl) "" ; ToDo: See also UBL CCL.
          :else "unknown")))

;;; ToDo: Make this a multi-method (on the case values)
(defn schema-type
  "Return a keyword signifying the specification of which the schema is part.
   These are #{:ubl-2, :oagis-10, etc.}. It is used as :schema/spec
   This is just a first guess; see schema/update-schema-type."
  [xmap]
  (let [ns (schema-ns xmap)
        sdo (:schema/sdo xmap)
        pname (:schema/pathname xmap)]
    (case sdo
      :cefact
      (cond (= ns "urn:un:unece:uncefact:data:specification:CoreComponentTypeSchemaModule:2")
            :generic/unqualified-dtype-schema
            :else (log/warn "Cannot determine schema-type:" pname))

      :oasis
      (cond (= ns "urn:oasis:names:specification:ubl:schema:xsd:UnqualifiedDataTypes-2")
            :generic/unqualified-dtype-schema,
            (= ns "urn:oasis:names:specification:bdndr:schema:xsd:UnqualifiedDataTypes-1")
            :generic/unqualified-dtype-schema,
            (= ns "urn:oasis:names:specification:ubl:schema:xsd:QualifiedDataTypes-2")
            :generic/qualified-dtype-schema,
            (re-matches #"^urn:oasis:names:specification:ubl:schema:xsd:Common[\w,\-,\:]+Components\-2$" ns)
            :ccts/component-schema
            (re-matches #"^urn:oasis:names:specification:ubl:schema:xsd:\w+-2$" ns)
            :ccts/message-schema
            :else (log/warn "Cannot determine schema-type:" pname))

      :oagi ; no URNs; guess based on pathname.
      (cond (re-matches #".*Fields\.xsd$" pname) ; though it is in the "Components" directory
            :generic/unqualified-dtype-schema
            (re-matches #".+CodeLists.+" pname)
            :generic/code-list-schema
            (re-matches #".+Components.+" pname)
            :ccts/component-schema
            (re-matches #"^http://www.openapplications.org/oagis/10$" ns)
            :ccts/message-schema ; ToDo: Not quite correct.
            :else (do (log/warn "Cannot determine schema-type:" pname)
                      :generic/xsd-file))

      :etsi :generic/xsd-file

      :iso :iso/iso-20022-schema

      :qif
      (cond (re-matches #".*QIFApplications/.*.xsd$" pname) :generic/message-schema
            (re-matches #".*QIFLibrary/.*.xsd$"   pname)    :generic/library-schema
            :else (do (log/warn "Cannot determine schema-type:" pname)
                      :generic/xsd-file))
      ;; Default
      :generic/xsd-file)))

(declare schema-name-special schema-name-std)
(defn schema-name
  "Return the name of the schema object. This uses the XML content to determine one."
  [xmap]
  (let [
        pname (:schema/pathname xmap)]
    (if (re-matches #".*sources/misc/.*" pname)
      (schema-name-special xmap)
      (schema-name-std xmap))))

(defn schema-name-special
  [xmap]
  (let [sdo  (:schema/sdo xmap)
        ver  (:schema/version xmap)
        sver (:schema/subversion xmap)
        pname (:schema/pathname xmap)]
    (if-let [[_ person dir fname] (re-matches #".*sources/misc/([a-zA-Z0-9\-\_]+)/([a-zA-Z0-9\-\_]+)/([a-zA-Z0-9\-\_]+).xsd" pname)]
      (cl-format nil "urn:~A-~A.~A:~A.~A.~A" (name sdo) ver sver person dir fname)
      (do (log/warn "Could not determine special schema name.")
           "unknown/special"))))

(defn schema-name-std
  [xmap]
  (let [sdo  (:schema/sdo xmap)
        ver  (:schema/version xmap)
        sver (:schema/subversion xmap)
        ver-str (if (empty? sver) ver (str ver "." sver ))
        pname (:schema/pathname xmap)]
    (cond
      (= :oagi sdo)
      (if-let [[_ fname] (re-matches #".*Components/(\w+).xsd" pname)]
        (str "urn:oagis-" ver-str ":Components:" fname)
        (if-let [[_ fname] (re-matches #".*Model/Nouns/(\w+).xsd" pname)]
          (str "urn:oagis-" ver-str ":Nouns:" fname)
          (if-let [[_ fname] (re-matches #".*Common/ISO20022/(pain[0-9,\.]+).xsd" pname)]
            (str "urn:oagis-" ver-str ":Common:" fname)
            (if-let [name  (-> (xpath xmap :xsd/schema :xsd/element) :xml/attrs :name)]
              (str  "urn:oagis-" ver-str ":" name)
              (if-let [res-pname (-> pname (str/split #"/") last (str/split #"\.") first)]
                (do (log/warn "Using pathname to define OAGIS" ver-str "schema name:" res-pname)
                    (str "urn:oagis-" ver-str ":" res-pname))
                (do (log/warn "Could not determine OAGIS" ver-str "schema name.")
                    :mm/nil)))))),
        (= :qif sdo)
        (if-let [[_ fname] (re-matches #".*/QIFLibrary/(\w+).xsd" pname)]
          (str "urn:QIF-" ver-str ":Library:" fname)
          (if-let [[_ fname] (re-matches #".*/QIFApplications/(\w+).xsd" pname)]
            (str "urn:QIF-" ver-str ":Application:" fname)
            (do (log/warn "Could not determine QIF" ver-str "schema name.") :mm/nil)))

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
   Specifically, what was :ccts/message-schema might be be a :ccts/bie."
  [schema]
  (let [bie? (atom false)]
    (letfn [(b? [obj]
              (cond @bie? true
                    (map? obj)    (doseq [[k v] (seq obj)]
                                    (when (= k :cct/BusinessContext)
                                      (reset! bie? true))
                                    (b? v))
                    (vector? obj) (doall (map b? obj))))]
      (b? schema)
      ;(log/info "Final :schema/type changed?: " @bie?)
      (cond-> schema
        @bie? (assoc :schema/type :cct/bie)))))

(defn singleize
  [obj]
  (if (== 1 (count obj)) (first obj) obj))

;;; Where this is used I'm typically trying to finesse the output into somethign less
;;; clunky than the shape of XSD/XML. ToDo: Still experimental!
(defn merge-warn
  "Merge the argument maps but warn where any of them have the same keys (collisions),
   unless the keys are are on the ok-set. Values of collision keys are aggregated."
  [maps & {:keys [ok-set] :or {ok-set #{:has/docString :has/documentation}}}]
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
  (if-let [typ (-> db-schema+ k :db/valueType)]
    (try
      (if (#{:db.type/long :db.type/float :db.type/number, :db.type/double :db.type/boolean} typ)
        (let [val (read-string v)]
          (if (= val 'unbounded) -1 val))
        v)
      (catch Exception _e (log/warn "Could not read xsd attribute declared a number: k = " k " v = " v)))
      (log/warn "Unknown attribute type: " k)))

;;; ToDo: All of the things currently in ns "xsd" should be in a NS called "xsdAttr".
(defn xsd-attr-map
  "Return a map of attributes where
    (1) the keys of a few special ones are in the argument namespace (a string) and the rest are in 'xsd', and_
    (2) the values are converted to the right type, based on the database schema."
  [attr-map nspace]
  (reduce-kv
   (fn [m k v]
     (let [kk (if (#{:ref :name :id} k)
                (keyword nspace (name k))
                (keyword "xsd" (name k)))]
       (assoc m kk (xsd-value-type kk v))))
   {}
   attr-map))


(def diag (atom nil))

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
(defn list-schemas
  "Return a list of schema, by default they are sorted by 'topic'"
  [& {:keys [sdo sort?] :or {sort? true}}]
  (let [base-names
        (if sdo
          (d/q `[:find [?n ...] :where [?s :schema/name ?n] [?s :schema/sdo ~sdo]] @(connect-atm))
          (d/q '[:find [?n ...] :where [_ :schema/name ?n]] @(connect-atm)))]
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
  [schema-urn & {:keys [resolve? filter-set] :or {resolve? true filter-set #{:db/id #_:doc/docString}}}]
  (when-let [ent  (d/q `[:find ?ent .
                         :where [?ent :schema/name ~schema-urn]] @(connect-atm))]
    (cond-> (dp/pull @(connect-atm) '[*] ent)
      resolve? (du/resolve-db-id (connect-atm) filter-set))))

