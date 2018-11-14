(ns metabase.driver.sql-jdbc.initialize-dependencies
  (:require [metabase.plugins :as plugins]
            [metabase.util :as u]
            [metabase.util.i18n :refer [trs]])
  (:import [java.sql DriverManager SQLException]))

(defn- assert-class-available [driver klass]
  (when klass
    (try
      (Class/forName klass)
      (catch ClassNotFoundException _
        (throw
         (Exception. (str (trs "Failed to initialize {0} driver. Cannot find class {1}." driver klass))))))))

(defn- assert-subprotocol-registered [driver subprotocol]
  (when subprotocol
    (let [protocol (str "jdbc:" subprotocol)]
      (try
        (DriverManager/getDriver protocol)
        (catch SQLException _
          (throw
           (Exception. (str (trs "Failed to initialize {0} driver. Protocol {1} not registered with JDBC DriverManager."
                                 driver protocol)))))))))

(defn initialize-dependencies!
  {:style/indent 1}
  [driver & {:keys [assert-class assert-subprotocol]}]
  (plugins/add-resource-with-name-to-classpath! (format "metabase.dependencies.%s.jar" (name driver)))
  (assert-class-available driver assert-class)
  (assert-subprotocol-registered driver assert-subprotocol)
  :ok)
