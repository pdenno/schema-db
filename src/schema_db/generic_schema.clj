(ns schema-db.generic-schema
  "Functions to read XML to structures that the DB can use."
  (:require
   [clojure.string]
   [schema-db.db-util     :as dbu    :refer [xpath xpath- xml-type?]]
   [schema-db.schema      :as schema :refer [simple-xsd?]]
   [schema-db.schema-util :as su     :refer [xml-group-by *prefix* schema-sdo schema-spec schema-version schema-subversion schema-type schema-name schema-topic
                                             merge-warn xsd-attr-map singleize]]
   [taoensso.timbre                  :as log]))

(def debugging? (atom false))
(def diag (atom false))

(defmulti rewrite-xsd #'su/rewrite-xsd-dispatch)

(defn call-this [arg]
  (reset! diag arg))

(defmethod rewrite-xsd nil [obj & schema]
  (if schema
    (log/warn "No method for obj = " obj " schema = " schema)
    (log/warn "No method for obj = " obj))
  (call-this {:obj obj :schema schema})
  :failure/rewrite-xsd-nil-method)

(defn process-attrs-map
  [attrs-map]
  (reduce-kv (fn [res k v] (-> res (conj (name k)) (conj (str v)))) [] attrs-map))

;;; ToDo: I think it is pretty odd that we call process-attrs-map here, especially so because
;;;       sometimes specific attrs are mapped again, differently.
(defmacro defparse [tag [arg _props] & body]
  `(defmethod rewrite-xsd ~tag [~arg & ~'_]
     ;; Once *skip-doc-processing?* is true, it stays so through the dynamic scope of the where it was set.
     (when @debugging? (println "defparse tag = " ~tag))
       (let [result# (do ~@body)]
         (cond-> result#
           (:xml/attrs result#) (-> (assoc :xml/attributes (-> result# :xml/attrs process-attrs-map))
                                    (dissoc :xml/attrs))))))

(defparse :ignore/ignore
  ;; This is used, for example, for :xml/tags that aren't going to be processed, such as :ROOT/srt_StateCode (from Michael's QIF thing).
  [_xmap]
  nil)

(defn imported-schemas
  "Using the :xsd*/import, return a map of the prefixes used in the schema."
  [ischemas ns-info]
  (reduce (fn [res schema]
            (if-let [prefix (-> ns-info :u->ps (get schema) first)]
              (conj res {:import_prefix prefix :import_referencedSchema schema})
              (do (log/warn "No prefix for imported schema" schema) res)))
          []
          (map #(-> % :xml/attrs :namespace) ischemas)))

(defn root2xsd
  "ROOT is 'http://www.w3.org/2001/XMLSchema'; switch ROOT tags to :xsd*/whatever and remove 'ROOT' as a ns."
  [xmap]
  (assert (= "http://www.w3.org/2001/XMLSchema"
             (-> xmap :xml/ns-info :p->u (get "ROOT"))))
  (letfn [(r2xsd [obj]
            (cond (map? obj)                         (reduce-kv (fn [m k v]
                                                                  (if (and (keyword? k) (= "ROOT" (namespace k)))
                                                                    (assoc m (keyword "xsd" (name k)) (r2xsd v))
                                                                    (assoc m k                        (r2xsd v))))
                                                                {} obj)
                  (and (keyword? obj)
                       (= "ROOT" (namespace obj)))   (keyword "xsd" (name obj))
                  (vector? obj)                      (mapv r2xsd obj)
                  :else                              obj))]
    (-> (r2xsd xmap)
        (update-in [:xml/ns-info :p->u]  #(dissoc % "ROOT"))
        (update-in [:xml/ns-info :p->u]  #(assoc % "xsd" "http://www.w3.org/2001/XMLSchema"))
        (update-in [:xml/ns-info :u->ps] #(assoc % "http://www.w3.org/2001/XMLSchema" ["xsd"])))))

(defn switch-xsd
  "The switch for RM from namespaces to _ makes the use of xsd as a namespace confusing, since
   that's also part of the name of many database schema attributes. Here we switch everything
   in the file to use xsd*, which if you ever see it in rewritten data, you know its wrong."
  [xmap]
  (letfn [(sxd [obj]
            (cond (map? obj)      (reduce-kv (fn [m k v] (assoc m (sxd k) (sxd v))) {} obj)
                  (vector? obj)   (mapv sxd obj)
                  (keyword? obj)  (if (= "xsd" (namespace obj)) (keyword "xsd*" (name obj)) obj)
                  :else           obj))]
    (-> xmap
        sxd
        (update-in [:xml/ns-info :p->u]  #(-> % (assoc "xsd*" (get % "xsd")) (dissoc "xsd")))
        (assoc-in [:xml/ns-info :u->ps "http://www.w3.org/2001/XMLSchema"] ["xsd*"]))))

(defn read-clean
  "Return a map structure containing the :xml/content (cleaned-up) :ns-info and :schema info."
  [pathname]
  (let [xml (dbu/read-xml pathname)]
    (as-> xml ?xmap
      (if (= "http://www.w3.org/2001/XMLSchema"
             (-> ?xmap :xml/ns-info :p->u (get "ROOT")))
        (root2xsd ?xmap)
        ?xmap)
      (switch-xsd ?xmap)
      ;; These tend to build on previously defined ones.
      (assoc ?xmap :schema_sdo (schema-sdo ?xmap))
      (assoc ?xmap :schema_spec (schema-spec ?xmap))
      (assoc ?xmap :schema_version (schema-version ?xmap))
      (assoc ?xmap :schema_subversion (schema-subversion ?xmap))
      (assoc ?xmap :schema_type (schema-type ?xmap))
      (assoc ?xmap :schema_name (schema-name ?xmap))
      (assoc ?xmap :schema_topic (schema-topic ?xmap)))))

;;; (read-schema-file "data/testing/elena/Company A - Invoice.xsd")
(defn read-schema-file
  "Create map structure for the DB for the given file.
   This sets :schema_type, which determines what method rewrite-xsd executes.
   See su/rewrite-xsd-dispatch."
  [path]
  (-> path
      read-clean
      rewrite-xsd
      #_dbu/condition-form)) ; ToDo: Review this; it might be worth keeping.

;;;===============  File Level =========================================================
(defparse :generic/xsdFile
  ;; The new perspective is that these are all just XSD files, so I borrowed this from :oagis/message-schema
  ;; If that's wrong, see the old perspective in the repository!
  [xmap]
    (binding [*prefix* "model"]
    (let [[_ short-name?] (re-matches  #"[\w,\:\d]+\:(\w+)\-\d" (:schema_name xmap))   ; <======= 4th
          {:keys [import other _attrs]} (xml-group-by (xpath xmap :xsd*/schema) :xsd*/import)]
      (reset! diag other)
      (cond-> xmap
        short-name?  (assoc :schema_shortName short-name?)
        import       (assoc :schema_importedSchema (imported-schemas import (:xml/ns-info xmap)))
        other        (assoc :schema_content (->> other (mapv rewrite-xsd) singleize))
        true         (dissoc :xml/ns-info :xml/content)))))

;;; Because :std/message-schema handles imports, and process :xsd*/schema, it only comes through here
;;; if it doesn't go through :std/message-schema. Thus I do the import thing here too.
(defparse :xsd*/schema
  [xmap]
  (binding [*prefix* "model"]
    (let [{:keys [import other _attrs]} (xml-group-by (xpath xmap :xsd*/schema) :xsd*/import)]
      (cond-> xmap
        import     (assoc :schema_importedSchema (imported-schemas import (:xml/ns-info xmap)))
        other      (assoc :schema_content (->> other (mapv rewrite-xsd) singleize))
        true       (dissoc :xml/tag :xml/content)))))

(defparse :xsd*/simpleType
  [xmap]
  {(keyword (str *prefix* "_" "simpleType"))
   (->> xmap :xml/content (mapv rewrite-xsd) singleize)})

;;; See, for example, /opt/messaging/sources/OAGIS/10.8.4/ModuleSet/Model/Platform/2_7/Common/DataTypes/BusinessDataType_1.xsd
(defparse :xsd*/simpleContent
  [xmap]
  {(keyword (str *prefix* "_" "simpleContent"))
   (->> xmap :xml/content (mapv rewrite-xsd) singleize)})

;;; https://stackoverflow.com/questions/57020857/xsd-difference-between-complexcontent-and-sequence
;;; While both xs:complexContent and xs:sequence can appear as child elements of xs:complexType, they serve completely different purposes:
;;; Use xs:complexContent (with a xs:restriction or xs:extension child element) to define a type that restricts or extends another type.
;;; Use xs:sequence to indicate a sequential ordering of subordinate elements (or other subordinate grouping constructs).
(defparse :xsd*/complexContent
  [xmap]
  (when-not (== 1 (count (:xml/content xmap)))
    (log/warn "xsd:complexContent has many :xml/content.")
    {(keyword (str *prefix* "_" "complexContent"))
     (rewrite-xsd (-> xmap :xml/content first))}))

(defparse :xsd*/any
  [xmap]
  (let [attrs (:xml/attrs xmap)]
    (cond-> {:sp_xsdType :any}
      true (assoc :has_docString ; ToDo: Investigate later
                  (str (:namespace attrs) "|" (:processContents attrs)))
      (:minOccurs attrs) (assoc :sp_minOccurs (-> attrs :minOccurs keyword))
      (:maxOccurs attrs) (assoc :sp_maxOccurs (-> attrs :maxOccurs keyword)))))

(defparse :xsd*/group
  [xmap]
  (let [ref? (-> xmap :xml/attrs :ref)]
    (cond-> {:sp_xsdType :any}
      ref? (assoc :sp_ref ref?))))

(defparse :xsd*/sequence
  [xmap]
  (let [content (:xml/content xmap)]
    {:model_sequence (mapv rewrite-xsd content)}))

(defparse :xsd*/anyAttribute
  [xmap]
  (let [{:keys [other attrs]} (xml-group-by xmap)
        attrs-map (xsd-attr-map attrs "anyAttribute")
        content? (->> other (mapv rewrite-xsd) not-empty)]
  {:xsd_anyAttribute (if content?
                       (merge-warn (into [attrs-map] content?))
                       attrs-map)}))

 ;;; <xsd:attribute name="typeCode" type="CodeType_1E7368" id="oagis-id-f17dd1fe69dc4728b41eb8086a8621e3" />
(defparse :xsd*/attribute
  [xmap]
  {:model_attribute (xsd-attr-map (:xml/attrs xmap) "attribute")})

;;; In OAGIS and UBL schema, these are parsed by other means. Not so in QIF, where it is just text.
(defparse :xsd*/choice
  [xchoice]
  {:xsd_choice (mapv rewrite-xsd (:xml/content xchoice))})

;;; <xsd:attribute name="typeCode" type="CodeType_1E7368" id="oagis-id-f17dd1fe69dc4728b41eb8086a8621e3"/>
(defparse :xsd*/complexType
  [xmap]
  (assert (= :xsd*/complexType (:xml/tag xmap)))
  (let [old-prefix *prefix*]
    (binding [*prefix* "complexType"]
      (let [{:keys [attributes other attrs]} (xml-group-by xmap :xsd*/attribute)
            attrs? (-> (xsd-attr-map attrs "complexType") not-empty) ; ToDo: Could be other attrs.
            name?  (:complexType_name attrs?)
            id?    (:complexType_id attrs?)
            attributes? (->> attributes (mapv rewrite-xsd) singleize not-empty)
            other?      (->> other (mapv rewrite-xsd) not-empty)
            content (cond-> {}
                      name?       (assoc :complexType_name name?)
                      id?         (assoc :complexType_id   id?)
                      attributes? (assoc :complexType_attributes attributes?))]
        {(keyword (str old-prefix "_" "complexType"))
         (if other?
           (merge-warn (into [content] other?))
           content)}))))

(defparse :xsd*/include
  [xmap]
  {:mm_tempInclude (-> xmap :xml/attrs :schemaLocation)})

;;; (9) :xsd*/extend :xsd*/restrict don't have sequences; don't add one. Return the restriction.
(defn extend-restrict
  [xmap restrict|extend]
  (let [{:keys [enumeration other attrs]} (xml-group-by xmap :xsd*/enumeration)
        enums? (->> enumeration (mapv #(-> % :xml-attrs :value)) (filter not-empty) not-empty)
        other? (->> other (remove char?) (mapv rewrite-xsd) not-empty)
        attrs-map? (-> (xsd-attr-map attrs (name restrict|extend)) not-empty)
        attr-name (case restrict|extend :restrict :restriction_attributes :extend :extension_attributes)
        typ       (case restrict|extend :restrict :model_restriction :extend :model_extension)
        content? (cond-> {}
                   attrs-map? (assoc attr-name attrs-map?)
                   enums?     (assoc :model_enumeration enums?)
                   true       vector
                   true       not-empty)]
    {typ (merge-warn (if content?
                       (into content? (if other? other? []))
                       other?))}))

(defparse :xsd*/extension
  [xmap]
  (extend-restrict xmap :extend))

(defparse :xsd*/restriction
  [xmap]
  (extend-restrict xmap :restrict))

(defparse :xsd*/list
  [obj]
  {:xsd_listItemType (-> obj :xml/attrs :itemType str)})

;;; This one is for everything in the map schema/simple-xsd? {:xsd*/length  :number, ...}
;;; Note that the tag is unchanged.
(defparse :generic/simple-xsd-elem
  [obj]
  (let [tag (:xml/tag obj)]
    {(keyword (str "xsd_" (name tag)))
     (if (= :number (simple-xsd? tag))
       (-> obj :xml/attrs :value read-string)
       (-> obj :xml/attrs :value))}))

(defparse :xsd*/attributeGroup
  [xmap]
  (let [doc (-> xmap (xpath- :xsd*/annotation :xsd*/documentation) :xml/content)
        ref? (-> xmap :xml/attrs :ref)]
    {:xsd_attributeGroup
     (cond-> {}
       (not-empty doc)   (assoc :has_docString doc)
       ref?              (assoc :xsdAttrGroup_data ref?))}))

(defparse :xsd*/union
  [xmap]
  {:model_union
   (-> xmap :xml/attrs :memberTypes (clojure.string/split  #"\s+") vec)})

(defparse :xsd*/key
  [xmap]
  (when-let [name (-> xmap :xml/attrs :name)]
    {:xsd_key name}))

(defparse :xsd*/keyref
  [xmap]
  (when-let [name (-> xmap :xml/attrs :name)]
    {:xsd_keyRef name}))

(defparse :xsd*/unique  [xmap]
  (when-let [name (-> xmap :xml/attrs :name)]
    {:xsd_unique name}))
