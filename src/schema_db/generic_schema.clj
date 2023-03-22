(ns schema-db.generic-schema
  "Functions to read XML to structures that the DB can use."
  (:require
   [clojure.string]
   [schema-db.db-util     :as dbu    :refer [xpath xpath- xml-type?]]
   [schema-db.schema      :as schema :refer [simple-xsd?]]
   [schema-db.schema-util :as su     :refer [*prefix* schema-sdo schema-spec schema-version schema-subversion schema-type schema-name
                                             merge-warn xsd-attr-map]]
   [taoensso.timbre                  :as log]))

(def debugging? (atom false))
(def diag (atom false))

(defmulti rewrite-xsd #'su/rewrite-xsd-dispatch)

(defmethod rewrite-xsd nil [obj & schema]
  (if schema
    (log/warn "No method for obj/schema.")
    (log/warn "No method for obj."))
  (reset! diag {:obj obj :schema schema})
  :failure/rewrite-xsd-nil-method)

(defn process-attrs-map
  [attrs-map]
  (reduce-kv (fn [res k v] (-> res (conj (name k)) (conj (str v)))) [] attrs-map))

;;; ToDo: Have a parse state and indent
;;;     (when *debugging?*
;;;       (println (cl-format nil "~A==> ~A" (util/nspaces (* 3 (-> ~pstate :tags count))) ~tag)))
;;; Establishes rewrite-xsd methods.
(defmacro defparse [tag [arg props] & body]
  `(defmethod rewrite-xsd ~tag [~arg & ~'_]
     ;; Once *skip-doc-processing?* is true, it stays so through the dynamic scope of the where it was set.
     (when @debugging? (println "defparse tag = " ~tag))
     (binding [su/*skip-doc-processing?* (or su/*skip-doc-processing?* (:skip-doc-processing? ~props))]
       (let [[~arg doc-string#] (su/obj-doc-string ~arg)
             result# (do ~@body)]
         (cond-> result#
           (and doc-string# (map? result#))    (assoc :doc/docString doc-string#)
           (:xml/attrs result#)                (-> (assoc :xml/attributes (-> result# :xml/attrs process-attrs-map))
                                                   (dissoc :xml/attrs)))))))

(defmacro defparse [tag [arg props] & body]
  `(defmethod rewrite-xsd ~tag [~arg & ~'_]
     ;; Once *skip-doc-processing?* is true, it stays so through the dynamic scope of the where it was set.
     (when @debugging? (println "defparse tag = " ~tag))
       (let [result# (do ~@body)]
         (cond-> result#
           (:xml/attrs result#)  (-> (assoc :xml/attributes (-> result# :xml/attrs process-attrs-map))
                                     (dissoc :xml/attrs))))))


(defn imported-schemas
  "Using the :xsd/import, return a map of the prefixes used in the schema."
  [xmap]
  (let [ischemas
        (->>
         (xpath xmap :xsd/schema)
         :xml/content
         (filter #(xml-type? % :xsd/import))
         (map #(-> % :xml/attrs :namespace)))
        ns-info (:xml/ns-info xmap)]
    (reduce (fn [res schema]
               (if-let [prefix (-> ns-info :u->ps (get schema) first)]
                 (conj res {:import/prefix prefix :import/referencedSchema schema})
                 (do (log/warn "No prefix for schema " schema) res)))
            []
            ischemas)))

(defn read-clean
  "Return a map structure containing the :xml/content (cleaned-up) :ns-info and :schema info."
  [pathname]
  (let [xml (dbu/read-xml pathname)]
    (as-> xml ?xmap
        (assoc ?xmap :schema/sdo (schema-sdo ?xmap))
        (assoc ?xmap :schema/spec (schema-spec ?xmap))
        (assoc ?xmap :schema/version (schema-version ?xmap))
        (assoc ?xmap :schema/subversion (schema-subversion ?xmap))
        (assoc ?xmap :schema/type (schema-type ?xmap))
        (assoc ?xmap :schema/name (schema-name ?xmap)))))

;;; (read-schema-file "data/testing/elena/Company A - Invoice.xsd")
(defn read-schema-file
  "Create map structure for the DB for the given file.
   This sets :schema/type, which determines what method rewrite-xsd executes.
   See su/rewrite-xsd-dispatch."
  [path]
  (-> path
      read-clean
      rewrite-xsd
      #_dbu/condition-form))

(defn call-this [arg]
  (reset! diag arg))

;;; ToDo: Pull attribute processing out of defparse and put it here???
(defn generic-rewrite
  "Call rewrite on :xml/content, either with mapv or singlely. dissoc :xml/content."
  [xmap]
  (let [content (:xml/content xmap)]
    (cond (vector? content) (if (> (count content) 1)
                              (mapv rewrite-xsd content)
                              (-> content first rewrite-xsd))
           (map? content)    (rewrite-xsd content),
           (string? content) content,
           :else (do (log/warn "Unexpected content: " content)
                     (call-this {:cnt content :xmap xmap})
                     content))))

;;;===============  File Level =========================================================
(defparse :generic/xsd-file
  ;; The new perspective is that these are all just XSD files, so I borrowed this from :oagis/message-schema
  ;; If that's wrong, see the old perspective in the repository!
  [xmap]
    (binding [*prefix* "model"]
    (-> (merge-warn [xmap (generic-rewrite xmap)])
        (dissoc :xml/ns-info :xml/content))))

(defparse :xsd/schema
  [xmap]
  (binding [*prefix* "model"]
    (-> xmap
        (assoc :schema/content (mapv rewrite-xsd (:xml/content xmap)))
        (dissoc :xml/tag :xml/content))))

(defparse :xsd/simpleType
  [xmap]
  {(keyword *prefix* "simpleType") (generic-rewrite xmap)})

;;; See, for example, /opt/messaging/sources/OAGIS/10.8.4/ModuleSet/Model/Platform/2_7/Common/DataTypes/BusinessDataType_1.xsd
(defparse :xsd/simpleContent
  [xmap]
  {(keyword *prefix* "simpleContent") (generic-rewrite xmap)})

;;; https://stackoverflow.com/questions/57020857/xsd-difference-between-complexcontent-and-sequence
;;; While both xs:complexContent and xs:sequence can appear as child elements of xs:complexType, they serve completely different purposes:
;;; Use xs:complexContent (with a xs:restriction or xs:extension child element) to define a type that restricts or extends another type.
;;; Use xs:sequence to indicate a sequential ordering of subordinate elements (or other subordinate grouping constructs).
(defparse :xsd/complexContent
  [xmap]
  (when-not (== 1 (count (:xml/content xmap)))
    (log/warn "xsd:complexContent has many :xml/content.")
    {(keyword *prefix* "complexContent") (rewrite-xsd (-> xmap :xml/content first))}))

(defparse :xsd/any
  [xmap]
  (let [attrs (:xml/attrs xmap)]
    (cond-> {:sp/xsdType :any}
      true (assoc :doc/docString ; ToDo: Investigate later
                  (str (:namespace attrs) "|" (:processContents attrs)))
      (:minOccurs attrs) (assoc :sp/minOccurs (-> attrs :minOccurs keyword))
      (:maxOccurs attrs) (assoc :sp/maxOccurs (-> attrs :maxOccurs keyword)))))

(defparse :xsd/group
  [xmap]
  (-> {:sp/xsdType :any}
      (assoc :sp/ref (-> xmap :xml/attrs :ref))))

(defparse :xsd/sequence
  [xmap]
  (let [content (:xml/content xmap)]
    {:model/sequence (mapv rewrite-xsd content)}))

;;; ToDo: better that this would be a set of things unhandled.
;;; Write them as :mm/unhandledXML (a stringified map).
(defparse :xsd/anyAttribute
  [xmap]
  (log/warn "anyAttribute = " xmap)
  {})

;;; <xsd:attribute name="typeCode" type="CodeType_1E7368" id="oagis-id-f17dd1fe69dc4728b41eb8086a8621e3" />
#_(defparse :xsd/attribute
  [obj]
  (let [attrs (:xml-attrs obj)
        required? (= "required" (:use attrs))]
    (cond-> {}
      (:name attrs) (assoc :sp/name (:name attrs)) ; ToDo: no name???
      (:type attrs) (assoc :sp/type (:type attrs)) ; Needs work, see :etsi
      required? (assoc :sp/minOccurs 1)
      required? (assoc :sp/maxOccurs 1)
      true (assoc :sp/xsdType :attribute))))

(defparse :xsd/attribute
  [xmap]
  {:model/attribute (xsd-attr-map (:xml/attrs xmap) "attribute")})

;;; In OAGIS and UBL schema, these are parsed by other means. Not so in QIF, where it is just text.
(defparse :xsd/choice
  [xchoice]
  {:xsd/choice (mapv rewrite-xsd (:xml/content xchoice))})

;;; <xsd:attribute name="typeCode" type="CodeType_1E7368" id="oagis-id-f17dd1fe69dc4728b41eb8086a8621e3"/>
(defparse :xsd/complexType
  [xmap]
  (assert (= :xsd/complexType (:xml/tag xmap)))
  (let [old-prefix *prefix*]
    (binding [*prefix* "complexType"]
      (let [attrs? (-> (xsd-attr-map (:xml/attrs xmap) "complexType") not-empty)
            name?  (:complexType/name attrs?)
            id?    (:complexType/id attrs?)
            attributes? nil #_(->> (:xml/content xmap) ; <========================================================
                                 (filter #(xml-type? % :xsd/attribute))
                                 generic-rewrite
                                 not-empty)
            other-content? (map rewrite-xsd (remove #(xml-type? % :xsd/attribute) (:xml/content xmap)))
            content (cond-> {}
                      name?       (assoc :complexType/name name?)
                      id?         (assoc :complexType/id   id?)
                      attributes? (assoc :complexType/attributes attributes?))]
        {(keyword old-prefix "complexType") (merge-warn (conj other-content? content))}))))

(defparse :xsd/include
  [xmap]
  {:mm/tempInclude (-> xmap :xml/attrs :schemaLocation)})

(defparse :xsd/extension
  [obj]
  (let [cnt (:xml/content obj)
        m (if cnt (rewrite-xsd cnt :extend-restrict-content) {})]
    (-> m
        (assoc :fn/type :extension)
        (assoc :fn/base (-> obj :xml/attrs :base)))))

;;; restrictions can have enumerations, factionDigits totalDigits minInclusive pattern minLength maxLength
(defparse :xsd/restriction
  [obj]
  (let [cnt (:xml/content obj)
        m (if (not-empty cnt) (rewrite-xsd cnt :extend-restrict-content) {})]
    (-> m
        (assoc :fn/type :restriction)
        (assoc :fn/base (-> obj :xml/attrs :base)))))

(defparse :xsd/list
  [obj]
  {:xsd/listItemType (-> obj :xml/attrs :itemType str)})

;;; This one is for everything in the mapschema/simple-xsd? {:xsd/length  :number, ...}
;;; Note that the tag is unchanged.
(defparse :simple-xsd
  [obj]
  (let [tag (:xml/tag obj)]
    {tag
     (if (= :number (simple-xsd? tag))
       (-> obj :xml/attrs :value read-string)
       (-> obj :xml/attrs :value))}))

;;; (9) :xsd/extend :xsd/restrict don't have sequences; don't add one. Return the restriction.
(defparse :extend-restrict-content
  [content]
  (let [enums (atom [])
        result (mapv #(if (xml-type? % :xsd/enumeration)
                        (swap! enums conj (-> % :xml/attrs :value))
                        (rewrite-xsd %))
                     content)]
    (if (not-empty @enums)
      {:model/enumeration @enums},
      {:temp/sequence result})))

(defparse :xsd/attributeGroup
  [xmap]
  (let [doc (-> xmap (xpath- :xsd/annotation :xsd/documentation) :xml/content)]
    {:xsd/attributeGroup
     (cond-> {}
       (not-empty doc)   (assoc :sp/docString doc)
       true              (assoc :xsdAttrGroup/data (-> xmap :xml/attrs :ref)))}))

(defparse :xsd/union
  [xmap]
  {:model/union
   (-> xmap :xml/attrs :memberTypes (clojure.string/split  #"\s+") vec)})
