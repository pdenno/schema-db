(ns schema-db.generic-schema
  "Functions to read XML to structures that the DB can use."
  (:require
   [schema-db.db-util     :as dbu    :refer [xpath xpath- xml-type?]]
   [schema-db.schema      :as schema :refer [simple-xsd?]]
   [schema-db.schema-util :as su     :refer [schema-sdo schema-spec schema-version schema-subversion schema-type schema-name]]
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

;;; Establishes rewrite-xsd methods.
(defmacro defparse [tag [arg props] & body]
  `(defmethod rewrite-xsd ~tag [~arg & ~'_]
     ;; Once *skip-doc-processing?* is true, it stays so through the dynamic scope of the where it was set.
     (when @debugging? (log/info "defparse tag = " ~tag))
     (binding [su/*skip-doc-processing?* (or su/*skip-doc-processing?* (:skip-doc-processing? ~props))]
       (let [[~arg doc-string#] (su/obj-doc-string ~arg)
             result# (do ~@body)]
         (if (and doc-string# (map? result#))
           (assoc result# :doc/docString doc-string#)
           result#)))))

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
      dbu/condition-form))

;;;===============  File Level =========================================================
;;; ToDo: Adapted from :oasis/component-schema, which suggests that that could use this.
;;; ToDo: This will prove to be woefully inadequate...later!
(defparse :generic/xsd-file
  [xmap]
  (let [{:xsd/keys [element complexType simpleType]}
        (-> (xpath xmap :xsd/schema) :xml/content dbu/child-types)
        schemas (-> xmap imported-schemas not-empty)]
    (as->
        (cond-> xmap
          true  (assoc  :schema/content [])
          element     (update :schema/content into
                              (mapv #(-> (rewrite-xsd % :xsd/element)
                                         (assoc :sp/function {:fn/type :type-ref}))
                                    element))
          complexType (update :schema/content into
                        (mapv (fn [cplx]
                                (as-> (rewrite-xsd cplx :xsd/complexType) ?t
                                 (if (:sp/type ?t) (assoc ?t :term/type (:sp/type ?t)) ?t)
                                 #_(update ?t :sp/function #(assoc % :fn/componentType cc-type))
                                 (dissoc ?t :sp/type)))
                              complexType))
          simpleType (update :schema/content into (mapv #(rewrite-xsd % :xsd/simpleType) simpleType))
          schemas (assoc :schema/importedSchemas schemas))
        ?r
      (if (-> ?r :schema/content empty?) (dissoc ?r :schema/content) ?r)
      (dissoc ?r :xml/ns-info :xml/content))))

;;; This is used at least for ISO 20022 "pain" schema. I assume one element
(defparse :xsd/simpleType
  [xmap]
  (if-not (== 1 (count (:xml/content xmap)))
    (log/warn "xsd:simpleType has many :xml/content.")
    (rewrite-xsd (-> xmap :xml/content first))))

(defparse :xsd/simpleContent
  [xmap]
  (when-not (== 1 (count (:xml/content xmap)))
    (log/warn "xsd:simpleContent has many :xml/content.")
    (rewrite-xsd (-> xmap :xml/content first))))

(defparse :xsd/complexContent
  [xmap]
  (when-not (== 1 (count (:xml/content xmap)))
    (log/warn "xsd:complexContent has many :xml/content.")
    {:model/complexType (rewrite-xsd (-> xmap :xml/content first))}))

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

(defparse :xsd/annotation
  [xmap]
  (->> (xpath- xmap :xsd/documentation)
       :xml/content
       not-empty))

(defparse :xsd/attribute
  [obj]
  (let [attrs (:xml-attrs obj)
        required? (= "required" (:use attrs))]
    (cond-> {}
      (:name attrs) (assoc :sp/name (:name attrs)) ; ToDo: no name???
      (:type attrs) (assoc :sp/type (:type attrs)) ; Needs work, see :etsi
      required? (assoc :sp/minOccurs 1)
      required? (assoc :sp/maxOccurs 1)
      true (assoc :sp/xsdType :attribute))))

;;; In OAGIS and UBL schema, these are parsed by other means. Not so in QIF, where it is just text.
(defparse :xsd/choice
  [xchoice]
  {:xsd/choice (mapv rewrite-xsd (:xml/content xchoice))})

;;; (0) This should create a :model/complexType (also don't create a :model/sequence child).
(defparse :xsd/complexType
  [xmap]
  (let [name (-> xmap :xml/attrs :name)]
    (as-> xmap ?r
      (cond (xpath ?r :xsd/complexContent)
            (-> (xpath ?r :xsd/complexContent) :xml/content first rewrite-xsd)
            (xpath ?r :xsd/simpleContent)
            (-> (xpath ?r :xsd/simpleContent) :xml/content first rewrite-xsd)
            :else
            {:temp/sequence-1 (->> (mapcat rewrite-xsd (:xml/content ?r)) vec)})
      (if name (assoc ?r :sp/name name) ?r)
      (if (= "true" (-> xmap :xml/attrs :abstract))
        (assoc ?r :sp/abstract true)
        ?r)
      {:model/complexType ?r})))

;;; This just returns the string pointing to a .xsd file. Those will be replaced in postprocessing.
(defparse :xsd/include
  [xmap]
  (-> xmap :xml/attrs :schemaLocation))

(defparse :xsd/extension
  [obj]
  (let [cnt (:xml/content obj)
        m (if cnt (rewrite-xsd cnt :extend-restrict-content) {})]
    (assoc m :sp/function {:fn/type :extension :fn/base (-> obj :xml/attrs :base)})))

;;; restrictions can have enumerations, factionDigits totalDigits minInclusive pattern minLength maxLength
(defparse :xsd/restriction
  [obj]
  (let [cnt (:xml/content obj)
        m (if cnt (rewrite-xsd cnt :extend-restrict-content) {})]
    (assoc m :sp/function {:fn/type :restriction :fn/base (-> obj :xml/attrs :base)})))

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
