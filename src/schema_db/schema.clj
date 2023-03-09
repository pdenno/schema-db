(ns schema-db.schema
  (:require
   [clojure.spec.alpha :as s]))

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
