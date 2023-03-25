(ns schema-db.schema
  (:require
   [clojure.spec.alpha :as s]))

(def db-schema+
  "Defines information for the datahike schema plus additional information about the property in :mm/info"
  {;; ---------------------- attribute (ToDo: another way? simpler?)
   :attribute/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string}
   :attribute/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string}
   ;; ---------------------- cct (core components). The capitalization here is like in the CCTS spec.
   :cct/ACCDefinition
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :mm/info {:usage [:oagis]}},
   :cct/ACCRevisionNumber
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long, :mm/info {:usage [:oagis]}},
   :cct/ACC_GUID
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity, :mm/info {:usage [:oagis]}},
   :cct/AlternativeBusinessTerms
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :mm/info {:usage [:oagis]}},
   :cct/ASCCDefinition
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :mm/info {:usage [:oagis]}},
   :cct/ASCCPDefinition
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :mm/info {:usage [:oagis] :property? true}},
   :cct/ASCCRevisionNumber
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long, :mm/info {:usage [:oagis]}},
   :cct/ASCCPRevisionNumber
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long, :mm/info {:usage [:oagis]  :property? true}},
   :cct/ASCC_GUID
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity, :mm/info {:usage [:oagis]}},
   :cct/ASCCP_GUID
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity, :mm/info {:usage [:oagis]}},
   :cct/AssociatedObjectClass
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :mm/info {:usage [:ubl]}}
   :cct/BCCDefinition
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :mm/info {:def? true, :usage [:oagis]}},
   :cct/BCCPDefinition
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :mm/info {:usage [:oagis] :property? true}},
   :cct/BCCRevisionNumber
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long, :mm/info {:usage [:oagis]}},
   :cct/BCCPRevisionNumber
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long, :mm/info {:usage [:oagis] :property? true}},
   :cct/BCC_GUID
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity, :mm/info {:usage [:oagis]}},
   :cct/BCCP_GUID
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity,
        :mm/info {:usage [:oagis] :property? true}},
   :cct/BIEEntityTypeCode
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :mm/info {:usage [:michael]}},
   :cct/BusinessContext
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref, :mm/info {:usage [:oagis]}},
   :cct/Cardinality
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/CategoryCode
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :cct/Component,
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref :mm/info {:usage [:ubl]}},
   :cct/ComponentType
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :mm/info {:usage [:ubl]}}
   :cct/ContentComponentValueDomain,
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
    :cct/DataType
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :mm/info {:usage [:ubl]}}
   :cct/DataTypeQualifier
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :mm/info {:usage [:ubl]}}
   :cct/DataTypeTermName
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/DefaultIndicator
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/Definition
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   #_#_:cct/Description
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/DictionaryEntryName
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/Examples
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :mm/info {:usage [:ubl]}}
   :cct/GUID #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity}
   :cct/Name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "Used in :cct/cctsBusinessContext, at least"},
   :cct/ObjectClass
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/ObjectClassTermName
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/PrimitiveType
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :cct/PrimitiveTypeName
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/PropertyTerm
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :mm/info {:usage [:ubl]}}
   :cct/PropertyTermName
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/PropertyTermQualifier
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :mm/info {:usage [:ubl]}}
   :cct/QualifierTerm
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/RepresentationTerm
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :mm/info {:usage [:ubl]}}
   :cct/RepresentationTermName
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   #_#_:cct/SchemeOrListID
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   #_#_:cct/SchemeOrListVersionID
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   #_#_:cct/SchemeOrListAgencyID
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   #_#_:cct/SchemeOrListModificationAllowedIndicator
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/UniqueID
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/UsageRule
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :cct/VersionID
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   #_#_:cct/scId
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "'sc' is supplementary to component"},
   #_#_:cct/scType
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   #_#_:cct/scUse
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   #_#_:cct/supplementary
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   #_#_:cct/SupplementaryComponentValueDomain,
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   ;; ------------------ codeList
   :codeList/id
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref :unique :db.unique/identity},
   #_#_:codeList/lists
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :codeList/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :codeList/terms
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :codeList/restriction
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref},
   :codeList/union
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref},
   ;; ------------------ complexType (a structured type such as :xsd/complexType)
   #_#_:complexType/attributes
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :complexType/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity
        :doc "unique ID for this complexType."}
   :complexType/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :complexType/simpleContent
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref},
   ;; ------------------ component (a structured type such as :xsd/complexType)
   :component/complexType
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   #_#_:component/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity
        :doc "unique ID for this complexType."}
   #_#_:component/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   ;; ------------------ element
   :element/complexType
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :element/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity
        :doc "unique ID for this element."}
   :element/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string,
        :doc "name of the element as in a xml tag."}
   :element/ref
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string,
        :doc "Reference to an :element/id"}
   #_#_:element/typeDef ; ToDo: Possibly more "typeDefs", each in its own NS.
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "Content of an element in the XML/XSD sense, including documentation."}
   ;; ----------------- extension
   :extension/attributes
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   ;; ----------------- fn (function, the purpose something serves)
   #_#_:fn/base
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   #_#_:fn/componentType
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   #_#_:fn/type
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   ;; ------------------ has (properties than occur in many situations)
   :has/docString
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :has/documentation
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :has/source
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   ;; ----------------- import
   :import/prefix
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :import/referencedSchema
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   ;; ----------------- iso
   #_#_:iso/CodeName #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "Used in OAGIS/ISO currency codes"},
   ;; ----------------- mm (used for metamodeling, note keeping in processing)
   :mm/comment
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "All the mm things are for debugging."},
   #_#_:mm/debug
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :mm/fileNotRead?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean},
   :mm/tempInclude
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string
        :doc "Despite the name, these are stored in the DB. In processing after all files
              are stored, we do d/q looking for these and resolving them by adding a :schema/includedSchema." }
   #_#_:mm/unhandledXML
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   ;; ------------------ model (general modeling concepts)
   :model/attribute
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :model/codeList
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref}
   #_#_:model/complexContent
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "something like xsd:complexContent that can be extended or restricted"}
   :model/complexType
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "something with structural elements"}
   #_#_:model/elementAttr
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "An attribute in the XML sense"}
   :model/elementDef
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "Names and element; possession of this property is part of an element defined in the object"}
   :model/elementRef
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "Names and element; possession of this property means there is such an element"}
   #_#_:model/enumeration
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string},
   #_#_:model/inlinedTypedef
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :model/sequence
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref, :doc "generic modeling concept"},
   #_#_:model/spec
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
        :doc "The model (definition of structure, constraints, and documentation) defined by this schema.
              This top-level, similar to :schema/name etc."},
   :model/simpleType
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "something with without internal structure, often known by its value"}
   :model/union
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string},
   :model/restriction
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :model/extension
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   ;; ------------------ restriction
   :restriction/attributes
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   ;; ------------------ schema (message schema level concepts)
   :schema/attributes
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref}
   :schema/codeList
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref}
   :schema/content
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "typically this includes the entire content of an xsd file. It found is a map that classifies the schema with e.g :schema/name, sdo etc."},
   :schema/importedSchema
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   #_#_:schema/includedSchema
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string},
   :schema/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity},
   :schema/pathname
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :schema/sdo
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :schema/shortName
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
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
   ;; -------------------- spec (characteristics of a standard or unit thereof (like a schema)-----

   ;; -------------------- sp (schema property)
   #_#_:sp/abstract
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean},
   #_#_:sp/componentDoc
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "This is typically structured content referencing CCT concepts (e.g. BContext, Component def, etc\n
              ToDo: It might be reasonable to merge the value of this into the parent and eliminate the property. "},
   #_#_:sp/docString
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,
      :doc "when :xsd/documentation is a string, rather than :sp/supplementary\n ToDo: Should I just use :doc/docString ?"},
   :sp/maxOccurs
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :sp/minOccurs
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   #_#_:sp/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :sp/ref
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "e.g. xsd:element attr."},
   #_#_:sp/supplementary
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref, :doc "CCT"},
   #_#_:sp/type
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :sp/xsdType
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword},
   :term/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "An ID unique within the schema"},
   ;; --------------- xsd (XML Schema concepts)
   #_#_:xml/attributes
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string,
        :doc "an order vector of <attr name> <attr value>"}, ; ToDo: Fix this. Order not guaranteed.
   #_#_:xml/rootNamespace
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "The URI of the un-prefixed namespace, if any"},
   ;; --------------- xsd (XML Schema concepts)
   :xsd/abstract
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/boolean},
   :xsd/base
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string},
   :xsd/anyAttribute
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref},
   :xsd/attributeFormDefault
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :xsd/attributeGroup
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :xsd/choice
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref},
   :xsd/default
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :xsd/elementFormDefault
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :xsd/fixed
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   #_#_:xsd/fractionDigits ; ?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long},
   :xsd/id ; ?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :xsd/key
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :xsd/keyRef
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :xsd/length ; ?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long},
   #_#_:xsd/name ; ?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :xsd/listItemType
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :doc "This is the itemType attr of the element."},
   :xsd/maxExclusive ; ?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/number},
   :xsd/maxInclusive ; ?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/number},
   #_#_:xsd/maxLength ; ?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long},
   :xsd/minExclusive ; ?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/number},
   :xsd/minInclusive ; ?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/number},
   #_#_:xsd/minLength ; ?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long},
   :xsd/mixed ; ? ToDo: This (and maybe :xsd/id) are the only ones where I get an error: (said to be not used, but it is).
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean},
   :xsd/maxOccurs
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long},
   :xsd/minOccurs
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long},
   :xsd/namespace
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :xsd/nillable
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean},
   :xsd/pattern
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   #_#_:xsd/ref ; ?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   #_#_:xsd/restriction ; ?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref},
   :xsd/substitutionGroup
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   #_#_:xsd/totalDigits ; ?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long},
   :xsd/targetNamespace
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :xsd/type
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :xsd/use
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :xsd/unique
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   :xsd/version
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string},
   ;;---------------------------------------------
   :xsdAttrGroup/data
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string},
   ;; ------------- zip (used for code lists)
   #_#_:zip/keys
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string},
   #_#_:zip/vals
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref}})

(def db-schema
  "Create a Datahike-compatible schema from the above."
  (reduce-kv (fn [r k v]
               (conj r (-> v
                           (dissoc :mm/info)
                           (assoc :db/ident k))))
             []
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

:ROOT/ccts_BasedASCCPDefinition

(def cct-special-cases
  #:ROOT{:ccts_ContentComponentValueDomain :cct/ContentComponentValueDomain,
         :ccts_sc-id                    :cct/scId,
         :ccts_sc-type                  :cct/scType,
         :ccts_sc-use                   :cct/scUse,
         :ccts_BasedACC_GUID            :cct/ACC_GUID
         :ccts_BasedASCC_GUID           :cct/ASCC_GUID
         :ccts_BasedASCCP_GUID          :cct/ASCCP_GUID
         :ccts_BasedBCC_GUID            :cct/BCC_GUID
         :ccts_BasedBCCP_GUID           :cct/BCCP_GUID
         :ccts_BasedASCCPRevisionNumber :cct/ASCCPRevisionNumber
         :ccts_BasedBCCPRevisionNumber  :cct/BCCPRevisionNumber
         :ccts_BusinessContext          :cct/BusinessContext
         :ccts_BasedASCCRevisionNumber  :cct/ASCCRevisionNumber
         :ccts_BasedACCRevisionNumber   :cct/ACCRevisionNumber
         :ccts_BasedBCCRevisionNumber   :cct/BCCRevisionNumber
         :ccts_BasedASCCDefinition      :cct/ASCCDefinition
         :ccts_BasedASCCPDefinition     :cct/ASCCPDefinition
         :ccts_BasedACCDefinition       :cct/ACCDefinition
         :ccts_BasedBCCDefinition       :cct/BCCDefinition
         :ccts_BasedBCCPDefinition      :cct/BCCPDefinition})

(def cct-tag2db-ident-map
  "Translate names of properties found in standard schema to the equivalent used in the database."
  (let [db-key-names (->> db-schema+ keys (filter #(= "cct" (namespace %))))
        tag-names (map #(keyword "ROOT" (str "ccts_" (name %))) db-key-names)]
    (merge (zipmap tag-names db-key-names) cct-special-cases)))

(def cct-obj?
  (->> db-schema+ keys (filter #(= "cct" (namespace %))) set))

;;; ToDo: These are mostly obsolete now!
;;; NB s/every-kv and s/every are probabilistic; they do not check every entry.
;;; ToDo: Write something that uses these and does d/q to check the whole DB. Of course, these need more work!
(s/def ::db-ent (s/keys :req [:db/id]))
(s/def ::type-ref (s/and ::db-ent (s/keys :req [:sp/name :sp/type])))
(s/def ::tagged (s/or :generic-elem ::gelem :component ::basic))
(s/def ::basic (s/and ::db-ent (s/keys :req [:sp/name :fn/type]) #(= :BBIE    (:fn-type %))))
(s/def ::gelem (s/and ::db-ent (s/keys :req [:sp/name :fn/type]) #(= :element (:fn-type %))))
(s/def ::quantified-elem (s/and ::gelem (s/keys :req [:sp/minOccurs :sp/maxOccurs])))
(s/def ::gelems (s/every ::gelem))
(s/def ::model-seq (s/and ::db-ent (s/keys :req [:model/sequence]) #(s/valid? ::gelems (:model/sequence %)))) ; Test a property!
(s/def ::ccts-based-message-schema (s/and ::db-ent (s/keys :req [:schema/type]) #(= :ccts/message-schema (:schema/type %))))
(s/def ::schema-type-kw #(or (special-schema-type? %)
                             (generic-schema-type? %)))
