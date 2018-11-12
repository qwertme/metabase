(ns metabase.test.data.sql-jdbc
  "Common test extension functionality for SQL-JDBC drivers."
  (:require [clojure.java.jdbc :as jdbc]
            [metabase
             [driver :as driver]
             [util :as u]]
            [metabase.test.data
             [datasets :as datasets]
             [interface :as tx]
             [sql :as sql.tx]]
            [metabase.test.data.sql-jdbc.load-data :as load-data]
            [clojure.tools.logging :as log]))

(driver/register! :sql-jdbc/test-extensions, :abstract? true)

(sql.tx/add-test-extensions! :sql-jdbc/test-extensions)

(defn add-test-extensions! [driver]
  (driver/add-parent! driver :sql-jdbc/test-extensions)
  (println "Added SQL JDBC test extensions for" driver "➕"))

(defmethod tx/create-db! :sql-jdbc/test-extensions [& args]
  (apply load-data/create-db! args))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                                    Util Fns                                                    |
;;; +----------------------------------------------------------------------------------------------------------------+

;; TODO - not sure if these belong here or somewhere else

(defn- do-when-testing-driver {:style/indent 1} [engine f]
  (require 'metabase.test.data.datasets)
  ((resolve 'metabase.test.data.datasets/do-when-testing-driver) engine f))

(defn execute-when-testing!
  "Execute prepared `sql-and-args` against Database with spec returned by `get-connection-spec` only when running tests
  against `driver`. Useful for doing driver-specific setup or teardown."
  {:style/indent 2}
  [driver get-connection-spec & sql-and-args]
  (datasets/when-testing-driver driver
    (println (u/format-color 'blue "[%s] %s" (name driver) (first sql-and-args)))
    (jdbc/execute! (get-connection-spec) sql-and-args)
    (println (u/format-color 'blue "[OK]"))))

(defn query-when-testing!
  "Execute a prepared `sql-and-args` **query** against Database with spec returned by `get-connection-spec` only when
  running tests against `driver`. Useful for doing driver-specific setup or teardown where `execute-when-testing!`
  won't work because the query returns results."
  {:style/indent 2}
  [driver get-connection-spec & sql-and-args]
  (datasets/when-testing-driver driver
    (println (u/format-color 'blue "[%s] %s" (name driver) (first sql-and-args)))
    (u/prog1 (jdbc/query (get-connection-spec) sql-and-args)
      (println (u/format-color 'blue "[OK] -> %s" (vec <>))))))
