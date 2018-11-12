(ns metabase.test.data.redshift
  (:require [metabase.driver.sql-jdbc.connection :as sql-jdbc.conn]
            [metabase.test.data
             [interface :as tx]
             [sql :as sql.tx]
             [sql-jdbc :as sql-jdbc.tx]]))

;; we don't need to add test extensions here because redshift derives from Postgres and thus already has test
;; extensions

;; Time, UUID types aren't supported by redshift
(defmethod sql.tx/field-base-type->sql-type [:redshift :type/BigInteger] [_ _] "BIGINT")
(defmethod sql.tx/field-base-type->sql-type [:redshift :type/Boolean]    [_ _] "BOOL")
(defmethod sql.tx/field-base-type->sql-type [:redshift :type/Date]       [_ _] "DATE")
(defmethod sql.tx/field-base-type->sql-type [:redshift :type/DateTime]   [_ _] "TIMESTAMP")
(defmethod sql.tx/field-base-type->sql-type [:redshift :type/Decimal]    [_ _] "DECIMAL")
(defmethod sql.tx/field-base-type->sql-type [:redshift :type/Float]      [_ _] "FLOAT8")
(defmethod sql.tx/field-base-type->sql-type [:redshift :type/Integer]    [_ _] "INTEGER")
(defmethod sql.tx/field-base-type->sql-type [:redshift :type/Text]       [_ _] "TEXT")

(def ^:private db-connection-details
  (delay {:host     (tx/db-test-env-var-or-throw :redshift :host)
          :port     (Integer/parseInt (tx/db-test-env-var-or-throw :redshift :port "5439"))
          :db       (tx/db-test-env-var-or-throw :redshift :db)
          :user     (tx/db-test-env-var-or-throw :redshift :user)
          :password (tx/db-test-env-var-or-throw :redshift :password)}))

(defmethod tx/dbdef->connection-details :redshift [& _]
  @db-connection-details)


;; Redshift is tested remotely, which means we need to support multiple tests happening against the same remote host
;; at the same time. Since Redshift doesn't let us create and destroy databases (we must re-use the same database
;; throughout the tests) we'll just fake it by creating a new schema when tests start running and re-use the same
;; schema for each test
(defonce ^:const session-schema-number
  (rand-int 240)) ; there's a maximum of 256 schemas per DB so make sure we don't go over that limit

(defonce ^:const session-schema-name
  (str "schema_" session-schema-number))

(defmethod sql.tx/create-db-sql         :redshift [& _] nil)
(defmethod sql.tx/drop-db-if-exists-sql :redshift [& _] nil)

(defmethod sql.tx/pk-sql-type :redshift [_] "INTEGER IDENTITY(1,1)")

(defmethod sql.tx/qualified-name-components :redshift [_ & names]
  (apply tx/single-db-qualified-name-components session-schema-name names))

(defmethod sql.tx/drop-table-if-exists-sql :redshift [& args]
  (apply sql.tx/drop-table-if-exists-cascade-sql args))


;;; Create + destroy the schema used for this test session

(defn- execute-when-testing-redshift! [format-str & args]
  (sql-jdbc.tx/execute-when-testing! :redshift
    (fn [] (sql-jdbc.conn/connection-details->spec :redshift @db-connection-details))
    (apply format format-str args)))

(defn- create-session-schema!
  {:expectations-options :before-run}
  []
  (execute-when-testing-redshift! "DROP SCHEMA IF EXISTS %s CASCADE; CREATE SCHEMA %s;" session-schema-name session-schema-name))

(defn- destroy-session-schema!
  {:expectations-options :after-run}
  []
  (execute-when-testing-redshift! "DROP SCHEMA IF EXISTS %s CASCADE;" session-schema-name))
