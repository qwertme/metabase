(ns metabase.test.data.sql-jdbc.load-data
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [honeysql
             [core :as hsql]
             [format :as hformat]
             [helpers :as h]]
            [medley.core :as m]
            [metabase
             [driver :as driver]
             [util :as u]]
            [metabase.driver.sql-jdbc.common :as sql-jdbc.common]
            [metabase.driver.sql.query-processor :as sql.qp]
            [metabase.test.data
             [interface :as tx]
             [sql :as sql.tx]]
            [metabase.test.data.sql-jdbc
             [execute :as execute]
             [spec :as spec]]
            [metabase.test.data.sql.ddl :as ddl]
            [metabase.util
             [date :as du]
             [honeysql-extensions :as hx]])
  (:import java.sql.SQLException))

(defmulti load-data!
  "Load the rows for a specific table into a DB. `load-data-chunked` is the default implementation (see below)."
  {:arglists '([driver dbdef tabledef])}
  tx/dispatch-on-driver-with-test-extensions
  :hierarchy #'driver/hierarchy)


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                                  Loading Data                                                  |
;;; +----------------------------------------------------------------------------------------------------------------+

;;; Loading Table Data

;; Since different DBs have constraints on how we can do this, the logic is broken out into a few different functions
;; you can compose together a driver that works with a given DB.
;;
;; (ex. SQL Server has a low limit on how many ? args we can have in a prepared statement, so it needs to be broken
;;  out into chunks; Oracle doesn't understand the normal syntax for inserting multiple rows at a time so we'll insert
;;  them one-at-a-time instead)


(defn load-data-get-rows
  "Get a sequence of row maps for use in a `insert!` when loading table data."
  [driver dbdef tabledef]
  (let [fields-for-insert (mapv (comp keyword :field-name)
                                (:field-definitions tabledef))]
    (for [row (:rows tabledef)]
      (zipmap fields-for-insert (for [v row]
                                  (if (and (not (instance? java.sql.Time v))
                                           (instance? java.util.Date v))
                                    (du/->Timestamp v du/utc)
                                    v))))))

(defn add-ids
  "Add an `:id` column to each row in `rows`, for databases that should have data inserted with the ID explicitly
  specified."
  [rows]
  (for [[i row] (m/indexed rows)]
    (assoc row :id (inc i))))

(defn load-data-add-ids
  "Add IDs to each row, presumabily for doing a parallel insert. This arg should go before `load-data-chunked` or
  `load-data-one-at-a-time`."
  [insert!]
  (fn [rows]
    (insert! (vec (add-ids rows)))))

(defn load-data-chunked
  "Insert rows in chunks, which default to 200 rows each."
  ([insert!]                   (load-data-chunked map insert!))
  ([map-fn insert!]            (load-data-chunked map-fn 200 insert!))
  ([map-fn chunk-size insert!] (fn [rows]
                                 (dorun (map-fn insert! (partition-all chunk-size rows))))))

(defn load-data-one-at-a-time
  "Insert rows one at a time."
  ([insert!]        (load-data-one-at-a-time map insert!))
  ([map-fn insert!] (fn [rows]
                      (dorun (map-fn insert! rows)))))

;; Alternative Impls

(declare do-insert!)

(defn make-load-data-fn
  "Create a `load-data!` function. This creates a function to actually insert a row or rows, wraps it with any
  WRAP-INSERT-FNS, the calls the resulting function with the rows to insert."
  [& wrap-insert-fns]
  (fn [driver {:keys [database-name], :as dbdef} {:keys [table-name], :as tabledef}]
    (jdbc/with-db-connection [conn (spec/database->spec driver :db dbdef)]
      (.setAutoCommit (jdbc/get-connection conn) false)
      (let [table-name (apply hx/qualify-and-escape-dots (sql.tx/qualified-name-components driver database-name table-name))
            insert!    ((apply comp wrap-insert-fns) (partial do-insert! driver conn table-name))
            rows       (load-data-get-rows driver dbdef tabledef)]
        (insert! rows)))))

(def load-data-all-at-once!            "Insert all rows at once."                             (make-load-data-fn))
(def load-data-chunked!                "Insert rows in chunks of 200 at a time."              (make-load-data-fn load-data-chunked))
(def load-data-one-at-a-time!          "Insert rows one at a time."                           (make-load-data-fn load-data-one-at-a-time))
(def load-data-add-ids!                "Insert all rows at once; add IDs."                    (make-load-data-fn load-data-add-ids))
(def load-data-chunked-parallel!       "Insert rows in chunks of 200 at a time, in parallel." (make-load-data-fn load-data-add-ids (partial load-data-chunked pmap)))
(def load-data-one-at-a-time-parallel! "Insert rows one at a time, in parallel."              (make-load-data-fn load-data-add-ids (partial load-data-one-at-a-time pmap)))
;; ^ the parallel versions aren't neccesarily faster than the sequential versions for all drivers so make sure to do some profiling in order to pick the appropriate implementation

;; Default imp

(defmethod load-data! :sql-jdbc/test-extensions [driver dbdef tabledef]
  (load-data-chunked driver dbdef tabledef))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                              CREATING DBS/TABLES                                               |
;;; +----------------------------------------------------------------------------------------------------------------+

(defn- escape-field-names
  "Escape the field-name keys in ROW-OR-ROWS."
  [row-or-rows]
  (if (sequential? row-or-rows)
    (map escape-field-names row-or-rows)
    (into {} (for [[k v] row-or-rows]
               {(sql-jdbc.common/escape-field-name k) v}))))

(defn- do-insert!
  "Insert ROW-OR-ROWS into TABLE-NAME for the DRIVER database defined by SPEC."
  [driver spec table-name row-or-rows]
  (let [prepare-key (comp keyword (partial sql.tx/prepare-identifier driver) name)
        rows        (if (sequential? row-or-rows)
                      row-or-rows
                      [row-or-rows])
        columns     (keys (first rows))
        values      (for [row rows]
                      (for [value (map row columns)]
                        (sql.qp/->honeysql driver value)))
        hsql-form   (-> (apply h/columns (for [column columns]
                                           (hx/qualify-and-escape-dots (prepare-key column))))
                        (h/insert-into (prepare-key table-name))
                        (h/values values))
        sql+args    (hx/unescape-dots (binding [hformat/*subquery?* false]
                                        (hsql/format hsql-form
                                          :quoting             (sql.qp/quote-style driver)
                                          :allow-dashed-names? true)))]
    (try (jdbc/execute! spec sql+args)
         (catch SQLException e
           (println (u/format-color 'red "INSERT FAILED: \n%s\n" sql+args))
           (jdbc/print-sql-exception-chain e)))))

(defn create-db!
  "Default implementation of `create-db!` for SQL drivers."
  {:arglists '([driver dbdef & {:keys [skip-drop-db?]}])}
  [driver {:keys [table-definitions], :as dbdef} & options]
  ;; first execute statements to drop/create the DB if needed (this will return nothing is `skip-drop-db?` is true)
  (doseq [statement (apply ddl/drop-db-ddl-statements driver dbdef options)]
    (execute/execute-sql! driver :server dbdef statement))
  ;; next, get a set of statements for creating the DB & Tables
  (let [statements (apply ddl/create-db-ddl-statements driver dbdef options)]
    ;; exec the combined statement
    (execute/execute-sql! driver :db dbdef (str/join ";\n" (map hx/unescape-dots statements))))
  ;; Now load the data for each Table
  (doseq [tabledef table-definitions]
    (du/profile (format "load-data for %s %s %s" (name driver) (:database-name dbdef) (:table-name tabledef))
      (load-data! driver dbdef tabledef))))
