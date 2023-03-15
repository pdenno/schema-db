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

;;; ToDo:
;;;    * The best thing to do with docs is alway collect them, but eliminate them in presentation like I optionally do with :db/id.
;;;    * elem-props-r only works for UBL schema.
;;;    * Get :sp/function into everything. BTW, CEFACT schema have  <ccts:Acronym>BBIE</ccts:Acronym>
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

;;; ToDo: Make the following environment variables.
(def ubl-root         (str src-dir "/OASIS/UBL-2.3/xsdrt/"))
(def oagis-10-8-root  (str src-dir "/OAGIS/10.8.4/ModuleSet/Model/"))
(def qif-root         (str src-dir "/QIF/3.0/xsd/"))
(def michael-root     (str src-dir "/michaelQIF/"))

(defonce bad-file-on-rebuild? (atom #{})) ; For debugging

(def diag-path "Pathname of error-producing file; for debugging; keep." (atom nil))

(defn add-schema-file!
  [path]
  (reset! diag-path path)
  (let [db-content (read-schema-file path)]
    (try
      (if (du/storable? db-content)
        (try (d/transact (connect-atm) db-content) ; Use d/transact here, not transact! which uses a future.
             (catch Exception e
               (swap! bad-file-on-rebuild? conj path)
               (log/error "Error adding" path ":" e)))
        (do (swap! bad-file-on-rebuild? conj path)
            (reset! diag db-content)
            (log/error "Schema-map contains nils and cannot be stored." path)))
      (catch Exception e
        (swap! bad-file-on-rebuild? conj path)
        (log/error "Error checking storable?" path ":" e)))))

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
      (d/transact (connect-atm) [{:schema/pathname file
                         :mm/fileNotRead? true}]))))

(defn bad-files
  "This creates a 'to do' list for debugging!"
  []
  (d/q '[:find [?p ...] :where
         [?ent :mm/fileNotRead? true]
         [?ent :schema/pathname ?p]]
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
(defn add-topics! []
  (let [forms (reduce (fn [forms urn]
                        (if-let [topic (su/schema-topic urn)]
                          (conj forms {:schema/name urn :schema/topic topic})
                          forms))
                      []
                      (su/list-schemas))]
    (d/transact (connect-atm) forms)))

(defn fix-includes! ; Just used on OAGIS schema, UBL-CommonExtensionComponents-2.3.xsd AFAIK.
  "Files have :mm/tempInclude which are paths (typically relative)
   Add :schema/includedSchemas where these are resolved to references to :schema/name."
  []
  (let [temps (d/q '[:find ?ent ?i ?p :keys s/ent s/include-file s/s-path :where
                     [?ent :mm/tempInclude ?i]
                     [?ent :schema/pathname ?p]]
                   @(connect-atm))
        with-urn (map (fn [temp]
                        (let [[_ up _ ipath] (re-matches #"^((\.\./)*)(.*)$" (:s/include-file temp))
                              schema-path (clojure.java.io/file (:s/s-path temp))
                              up-cnt (inc (int (/ (count up) 3)))
                              dir    (du/dir-up schema-path up-cnt)
                              file   (str dir "/" ipath)]
                          (if-let [urn (d/q `[:find ?urn . :where
                                              [?e :schema/pathname ~file]
                                              [?e :schema/name ?urn]]
                                            @(connect-atm))]
                            (assoc temp :s/include urn)
                            (do (log/warn "While resolving includes, cannot find schema with :schema/pathname" file)
                                temp))))
                      temps)]
    (d/transact (connect-atm)
                (mapv #(-> {}
                           (assoc :db/id (:s/ent %))
                           (assoc :schema/includedSchemas (:s/include %)))
                      (filter #(contains? % :s/include) with-urn)))))

(defn postprocess-schemas!
  "Do some additional work on schema already in the DB."
  []
  (add-topics!)
  (fix-includes!)
  (update-bad-files!))

;;;================================ Starting and Stopping ===========================================
;;; (user/restart) whenever you update the DB or the resolvers. (tools/refresh) if compilation fails.

(defn create-db!
  "Create the database if :rebuild? is true, otherwise just set the connection atom, conn."
  []
  (util/config-log :info)
  (when (:rebuild-db? @db-cfg-atm)
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
    ;;(add-schema-files! michael-root) ; Currently has nils
    (postprocess-schemas!)
    (log/info "Created schema DB")))

(defstate core
  :start
  (do
    (util/config-log :info)
    (reset! db-cfg-atm {:store {:backend :file :path db-dir}
                        :rebuild-db? false
                        :schema-flexibility :write})
    (log/info "Starting schema-db: db connection = " @(connect-atm))))
