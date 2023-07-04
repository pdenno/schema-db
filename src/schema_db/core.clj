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
   [datahike.api                 :as d]
   [mount.core                   :refer [defstate]]
   [schema-db.db-util            :as du     :refer [db-cfg-atm connect-atm]]
   [schema-db.generic-schema     :as gen-s  :refer [read-schema-file]]
   [schema-db.schema             :as schema :refer [db-schema]]
   [schema-db.schema-util        :as su]
   [schema-db.std-schema] ; Needed for mount
   [schema-db.util               :as util]
   [taoensso.timbre              :as log]))

;;; (1) Generally speaking, parse methods should return a map corresponding to the thing parsed;
;;;     it should not be left up to the caller to do that.
;;; (2) For convenience in debugging, I generate some temp stuff.
;;;     For example :temp/sequence are to be replaced by a their vector value, the elements of which are maps.
;;;     :model/sequence should imply a modeling intent such as from :xsd/sequence.
;;; (3) It is assumed that the generation of db.cardinality/many things is localized.
;;;     There aren't safeguards against assoc replacing a value already generated.
;;; (4) db attributes in the model namespace refer to generally-recognized modeling concepts in XSD and other data definition languages.
;;;     db attributes in the schema namespace refer to more specific modeling concepts of normative schema.

;;; ToDo:
;;;    * :fn/type :element is pretty worthless, can I do better?
;;;      -  Get :fn/type into everything. BTW, CEFACT schema have  <ccts:Acronym>BBIE</ccts:Acronym>
;;;    * The best thing to do with docs is alway collect them, but eliminate them in presentation like I optionally do with :db/id.
;;;    * elem-props-r only works for UBL schema.
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

(def diag (atom nil))

(def ignore-file?
  #{})

;;; ToDo: Make the following environment variables.
;;;(def ubl-root         (str src-dir "/OASIS/UBL-2.3/xsdrt/"))
(def ubl-root         (str src-dir "/OASIS/UBL-2.3/xsd/"))
(def oagis-10-8-root  (str src-dir "/OAGIS/10.8.4/ModuleSet/Model/"))
(def niem-root        (str src-dir "/NIEM/5.2/NIEM-Releases-niem-5.2/xsd/"))
(def qif-root         (str src-dir "/QIF/3.0/xsd/"))
(def michael-root     (str src-dir "/misc//michael/QIF/"))
(def elena-root       (str src-dir "/misc/elena/"))

(defonce bad-file-on-rebuild? (atom #{})) ; For debugging

(def diag-path "Pathname of error-producing file; for debugging; keep." (atom nil))

(defn add-schema-file!
  [path]
  (reset! diag-path path)
  (if (ignore-file? path)
    (log/warn "Ignoring file owing to it being on ignore list: " path)
    (try
      (let [db-content
            (-> path
                read-schema-file
                ;su/simplify-temps
                su/update-schema-type
                vector)]
        (reset! diag {:db-content db-content})
        (if (du/storable? db-content)
          (try (d/transact (connect-atm) db-content) ; Use d/transact here, not transact! which uses a future.
               (catch Exception e
                 (swap! bad-file-on-rebuild? conj path)
                 (log/error "Error adding" path ":" e)))
          (do (swap! bad-file-on-rebuild? conj path)
              (reset! diag db-content)
              (log/error "Schema-map contains nils or :xml/tag and cannot be stored." path))))
      (catch Exception e
        (swap! bad-file-on-rebuild? conj path)
        (log/error "Error in add-schema-file. path= " path ":" e)))))

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
  (when (:rebuild-db? @db-cfg-atm)
    (doseq [file @bad-file-on-rebuild?]
      (d/transact (connect-atm) [{:schema_pathname file
                         :mm_fileNotRead? true}]))))

(defn bad-files
  "This creates a 'to do' list for debugging!"
  []
  (d/q '[:find [?p ...] :where
         [?ent :mm_fileNotRead? true]
         [?ent :schema_pathname ?p]]
       @(connect-atm)))

(defn unused-attrs
  "Return a list of unused database attributes (for debugging)."
  []
  (let [unused (atom [])]
    (doseq [attr (map :db/ident db-schema)]
      (when (empty? (d/q `[:find ?e :where [?e ~attr ?]] @(connect-atm)))
        (swap! unused conj attr)))
    @unused))

;;; =================================== Schema-db post-processing ==============================
#_(defn add-topics! []
  (let [forms (reduce (fn [forms name]
                        (if-let [topic (su/schema-topic name)]
                          (conj forms {:schema_name name :schema_topic topic})
                          forms))
                      []
                      (su/list-schemas))]
    (d/transact (connect-atm) forms)))

(defn fix-includes! ; Just used on OAGIS schema, UBL-CommonExtensionComponents-2.3.xsd AFAIK.
  "Files have :mm_tempInclude which are paths (typically relative)
   Add :schema_includedSchemas where these are resolved to references to :schema_name."
  []
  (let [temps (d/q '[:find ?ent ?i ?p :keys s/ent s/include-file s/s-path :where
                     [?ent :mm_tempInclude ?i]
                     [?ent :schema_pathname ?p]]
                   @(connect-atm))
        with-urn (map (fn [temp]
                        (let [[_ up _ ipath] (re-matches #"^((\.\./)*)(.*)$" (:s/include-file temp))
                              schema-path (clojure.java.io/file (:s/s-path temp))
                              up-cnt (inc (int (/ (count up) 3)))
                              dir    (du/dir-up schema-path up-cnt)
                              file   (str dir "/" ipath)]
                          (if-let [urn (d/q `[:find ?urn . :where
                                              [?e :schema_pathname ~file]
                                              [?e :schema_name ?urn]]
                                            @(connect-atm))]
                            (assoc temp :s/include urn)
                            (do (log/warn "While resolving includes, cannot find schema with :schema_pathname" file)
                                temp))))
                      temps)]
    (d/transact (connect-atm)
                (mapv #(-> {}
                           (assoc :db/id (:s/ent %))
                           (assoc :schema_includedSchema (:s/include %)))
                      (filter #(contains? % :s/include) with-urn)))))

(defn postprocess-schemas!
  "Do some additional work on schema already in the DB."
  []
  #_(add-topics!)
  (fix-includes!)
  (update-bad-files!))

(def rebuild-db? "Don't keep this on the db-cfg map." true)

(defn create-db!
  "Create the database if :rebuild? is true."
  []
  (util/config-log :info) ; Prevent DH debugging messages.
  (when rebuild-db?
    (reset! bad-file-on-rebuild? #{})
    (when (d/database-exists? @db-cfg-atm) (d/delete-database @db-cfg-atm))
    (d/create-database @db-cfg-atm)
    (d/transact (connect-atm) db-schema)
    (add-schema-files! (str ubl-root "maindoc"))
    (add-schema-files! (str ubl-root "common"))
    (add-schema-files! (str oagis-10-8-root "Nouns"))
    (add-schema-files! (str oagis-10-8-root "Platform/2_7/Common"))
    (add-schema-files! (str qif-root "QIFApplications"))
    (add-schema-files! (str qif-root "QIFLibrary"))

    (add-schema-files! (str elena-root "2023-02-09"))
    (add-schema-files! (str elena-root "2023-07-02"))
    (add-schema-files! (str niem-root "codes"))
    (add-schema-files! (str niem-root "domains"))
    (add-schema-files! (str niem-root "utility"))
    (add-schema-file!  (str niem-root "niem-core.xsd"))

    ;(add-schema-files! michael-root)
    (postprocess-schemas!)
    (log/info "Created schema DB")))

;;;================================ Starting and Stopping ===========================================
;;; (user/restart) whenever you update the DB or the resolvers. (tools/refresh) if compilation fails.
(defstate core
  :start
  (do
    (util/config-log :info)
    (reset! db-cfg-atm {:store {:backend :file :path db-dir}
                        :keep-history? false
                        :schema-flexibility :write})
    (log/info "Starting schema-db: db connection = " @(connect-atm))))
