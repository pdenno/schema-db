(ns schema-db.schema
  (:require
   [clojure.spec.alpha :as s]))

(def db-schema+
  "Defines information for the datahike schema plus additional information about the property in :mm/info"
  {:cct/ACCDefinition
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :mm/info {:fn :ACC, :def? true, :score? true}},
   :cct/ACCRevisionNumber
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long, :mm/info {:fn :ACC, :score? true}},
   :cct/ACC_GUID
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity, :mm/info {:fn :ACC, :score? true}},
   :cct/ASCCDefinition
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :mm/info {:fn :ASCC, :def? true, :score? true}},
   :cct/ASCCRevisionNumber
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long, :mm/info {:fn :ASCC, :score? true}},
   :cct/ASCC_GUID
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity, :mm/info {:fn :ASCC, :score? true}},
   :cct/BCCDefinition
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :mm/info {:fn :BCC, :def? true, :score? true}},
   :cct/BCCRevisionNumber
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long, :mm/info {:fn :BCC, :score? true}},
   :cct/BCC_GUID
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity, :mm/info {:fn :BCC, :score? true}},
   :cct/BusinessContext
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref, :mm/info {:fn :BusinessContext, :score? true}},
   :cct/Cardinality
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/CategoryCode
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :cct/DataTypeTermName
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/Definition
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/Description
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/DictionaryEntryName
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/GUID #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity}
   :cct/Name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "Used in :cct/cctsBusinessContext, at least"},
   :cct/ObjectClass
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/PrimitiveType
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :cct/PropertyTermName
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/QualifierTerm
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/RepresentationTermName
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/UniqueID
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/UsageRule
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/VersionID
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/scId
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "'sc' is supplementary to component"},
   :cct/scType
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :cct/scUse
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :cct/supplementary
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   ;; ------------------ codeList
   :codeList/lists
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :codeList/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :codeList/terms
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   ;; ------------------ doc
   :doc/docString #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   ;; ----------------- fn
   :fn/base
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :fn/componentType
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :fn/type
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   ;; ----------------- import
   :import/prefix
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :import/referencedSchema
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   ;; ----------------- iso
   :iso/CodeName #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "Used in OAGIS/ISO currency codes"},
   ;; ----------------- mm (used for metamodeling, note keeping in processing)
   :mm/comment
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "All the mm things are for debugging."},
   :mm/debug
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :mm/fileNotRead?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean},
   :mm/tempInclude
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string},
   :mm/unhandledXML
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   ;; ------------------ model (general modeling concepts)
   :model/complexType
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :model/enumeration
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string},
   :model/namedElement
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref, :doc "Something names, such as xsd:element with :ref or :name."},
   :model/sequence
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref, :doc "generic modeling concept"},
   ;; ------------------ schema (message schema level concepts)
   :schema/content
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref, :doc "typically this includes the entire content of an xsd file."},
   :schema/importedSchemas
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :schema/includedSchemas
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string},
   :schema/inlinedTypedefs
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :schema/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity},
   :schema/pathname
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :schema/sdo
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :schema/shortName
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :schema/simpleTypes
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :schema/spec
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :schema/subversion
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "e.g. for OAGIS 10.8 it is '8'"},
   :schema/topic
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :schema/type
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :schema/version
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "e.g. for OAGIS 10.8 it is '10'"},
   ;; -------------------- sp (schema property)
   :sp/abstract
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean},
   :sp/componentDoc
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "This is typically structured content referencing CCT concepts (e.g. BContext, Component def, etc\n
              ToDo: It might be reasonable to merge the value of this into the parent and eliminate the property. "},
   :sp/docString
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,
      :doc "when :xsd/documentation is a string, rather than :sp/supplementary\n ToDo: Should I just use :doc/docString ?"},
   :sp/function
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref, :doc ":fn objects."},
   :sp/maxOccurs
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :sp/minOccurs
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :sp/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :sp/ref
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "e.g. xsd:element attr."},
   :sp/supplementary
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref, :doc "CCT"},
   :sp/type
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :sp/typeDef
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref, :doc "xsd:elem with a name, type attrs"},
   :sp/xsdType
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :term/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "An ID unique within the schema"},
   :xml/rootNamespace
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "The URI of the un-prefixed namespace, if any"},
   ;; --------------- xsd (XML Schema concepts)
   :xsd/attributeGroup
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :xsd/choice
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :xsd/fractionDigits
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long},
   :xsd/length
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long},
   :xsd/listItemType
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "This is the itemType attr of the element."},
   :xsd/maxExclusive
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/number},
   :xsd/maxInclusive
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/number},
   :xsd/maxLength
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long},
   :xsd/minExclusive
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/number},
   :xsd/minInclusive
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/number},
   :xsd/minLength
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long},
   :xsd/pattern
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :xsd/restriction
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref},
   :xsd/totalDigits
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long},
   :xsdAttrGroup/data
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string},
   ;; ------------- zip (used for code lists)
   :zip/keys
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string},
   :zip/vals
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref}})

(def db-schema
  "Create a Datahike-compatible schema from the above."
  (reduce-kv (fn [r k v]
               (conj r (-> v
                           (dissoc :mm/info)
                           (assoc :db/ident k))))
             {}
             db-schema+))

(def simple-xsd?
  {:xsd/length         :number
   :xsd/minLength      :number
   :xsd/maxLength      :number
   :xsd/pattern        :string
   :xsd/fractionDigits :number
   :xsd/totalDigits    :number
   :xsd/maxExclusive   :number
   :xsd/maxInclusive   :number
   :xsd/minExclusive   :number
   :xsd/minInclusive   :number})

(def generic-schema-type? "These might be associated with whole files, but specializations might exist"
  #{:generic/message-schema
    :generic/library-schema
    :generic/qualified-dtype-schema,
    :generic/unqualified-dtype-schema
    :generic/code-list-schema
    :generic/xsd-file})

(def special-schema-type? "These are associated with whole files."
  #{:ccts/message-schema
    :ubl/message-schema
    :oagis/message-schema
    :ccts/component-schema
    :oasis/component-schema
    :iso/iso-20022-schema})

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
(s/def ::schema-type-kw #(or (special-schema-type? %)
                             (generic-schema-type? %)))
