(ns metabase.test.data.datasets
  "Interface + implementations for loading test datasets for different drivers, and getting information about the
  dataset's tables, fields, etc.

  TODO - we should seriously rename this namespace to something like `metabase.test.driver` or something like that.
  Also need to stop using 'engine' to mean 'driver keyword'."
  (:require [clojure.string :as s]
            [clojure.tools.logging :as log]
            [colorize.core :as color]
            [environ.core :refer [env]]
            [expectations :refer [expect doexpect]]
            [metabase
             [driver :as driver]
             [util :as u]]
            [metabase.test.data.interface :as tx]))

;; # Logic for determining which datasets to test against

;; By default, we'll test against against only the :h2 (H2) dataset; otherwise, you can specify which
;; datasets to test against by setting the env var `DRIVERS` to a comma-separated list of dataset names, e.g.
;;
;;    # test against :h2 and :mongo
;;    DRIVERS=h2,mongo
;;
;;    # just test against :h2 (default)
;;    DRIVERS=h2

(defn- get-drivers-from-env
  "Return a set of drivers to test against from the env var `DRIVERS`."
  []
  (when (seq (env :engines))
    (println
     (u/format-color 'red
         "The env var ENGINES is no longer supported. Please specify drivers to run tests against with DRIVERS instead.")))
  (when-let [env-drivers (some-> (env :drivers) s/lower-case)]
    (set (for [engine (s/split env-drivers #",")
               :when engine]
           (keyword engine)))))

(defonce ^{:doc (str "Set of names of drivers we should run tests against. By default, this only contains `:h2` but can"
                     " be overriden by setting env var `DRIVERS`.")}
  test-drivers
  (let [drivers (or (get-drivers-from-env)
                    #{:h2})]
    (log/info (color/cyan "Running QP tests against these drivers: " drivers))
    drivers))

(defn- validate-test-drivers
  {:expectations-options :before-run}
  []
  (doseq [driver test-drivers]
    (tx/the-driver-with-test-extensions driver)))

;; # Helper Macros

(defn do-when-testing-driver
  "Call function F (always with no arguments) *only* if we are currently testing against ENGINE.
   (This does NOT bind `*driver*`; use `driver/with-driver` if you want to do that.)"
  {:style/indent 1}
  [driver f]
  (when (contains? test-drivers driver)
    (f)))

(defmacro when-testing-driver
  "Execute `body` only if we're currently testing against `driver`.
   (This does NOT bind `*driver*`; use `with-driver-when-testing` if you want to do that.)"
  {:style/indent 1}
  [driver & body]
  `(do-when-testing-driver ~driver (fn [] ~@body)))

(defmacro with-driver-when-testing
  "When `driver` is specified in `DRIVERS`, bins `*driver*` and executes `body`."
  {:style/indent 1}
  [driver & body]
  `(when-testing-driver ~driver
     (driver/with-driver ~driver
       ~@body)))

(defmacro expect-with-engine
  "Generate a unit test that only runs if we're currently testing against ENGINE, and that binds `*driver*` to the
  driver for ENGINE."
  {:style/indent 1}
  [driver expected actual]
  `(when-testing-driver ~driver
     (expect
       (driver/with-driver ~driver ~expected)
       (driver/with-driver ~driver ~actual))))

(defmacro expect-with-engines
  "Generate unit tests for all drivers in `DRIVERS`; each test will only run if we're currently testing the
  corresponding dataset. `*driver*` is bound to the current dataset inside each test."
  {:style/indent 1}
  [drivers expected actual]
  ;; Make functions to get expected/actual so the code is only compiled one time instead of for every single driver
  ;; speeds up loading of metabase.driver.query-processor-test significantly
  `(let [e# (fn [driver#] (driver/with-driver driver# ~expected))
         a# (fn [driver#] (driver/with-driver driver# ~actual))]
     (defn ~(vary-meta (symbol (str "expect-with-engines-" (hash &form)))
                       assoc :expectation true)
       []
       (doseq [driver# ~drivers]
         (when-testing-driver driver#
           (doexpect (e# driver#) (a# driver#)))))))

(defmacro expect-with-all-engines
  "Generate unit tests for all drivers specified in `DRIVERS`. `*driver*` is bound to the current driver inside each
  test."
  {:style/indent 0}
  [expected actual]
  `(expect-with-engines test-drivers ~expected ~actual))
