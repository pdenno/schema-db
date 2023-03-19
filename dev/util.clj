(ns dev.util)

(defn find-root
  "Walking through the object, collect
    - map keys in the 'ROOT' namespace, and
    - :xml/tag's that have a value that is ROOT."
  [obj & {:keys [ns-name] :or {ns-name "ROOT"}}]
  (let [res-atm (atom #{})]
    (letfn [(fr [obj]
              (cond (map? obj) (doseq [[k v] (seq obj)]
                                 (when (= ns-name (namespace k))
                                   (swap! res-atm conj k))
                                 (when (and (= k :xml/tag) (= ns-name (namespace v)))
                                   (swap! res-atm conj [:xml/tag v]))
                                 (fr v))
                    (vector? obj) (doall (map fr obj))))]
      (fr obj))
    @res-atm))

;;; Here is what I'v found so far:
#_#{[:xml/tag :ROOT/ccts_BasedACC_GUID]                      ; string content
     :ROOT/ccts_BasedASCCRevisionNumber                       ; string/number content
    :ROOT/ccts_BasedASCCDefinition                           ; ref ('source' attr and string content)
    [:xml/tag :ROOT/ccts_GUID]                               ; string content
    :ROOT/ccts_BusinessContext                               : ref
    [:xml/tag :ROOT/ccts_BasedACCRevisionNumber]             ; string/number content
    :ROOT/ccts_BasedBCCDefinition                            ; ref ('source' attr and string content)
    :ROOT/ccts_BasedBCC_GUID                                 ; string content
    [:xml/tag :ROOT/ccts_BasedACCDefinition]                 ; ref ('source' attr and string content)
    [:xml/tag :ROOT/ccts_Name]                               ; string content
    :ROOT/ccts_BasedASCC_GUID                                ; string content
    :ROOT/ccts_BasedBCCRevisionNumber}                       ; string/number content
