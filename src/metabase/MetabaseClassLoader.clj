(ns metabase.MetabaseClassLoader
  (:gen-class
   :extends clojure.lang.DynamicClassLoader
   :exposes-methods {findClass superFindClass}))

(defn -close []
  (println "You tried to close, but... NOPE! :D"))

(defn -findClass [this, ^String name]
  (println "find class:" name) ; NOCOMMIT
  (.superFindClass this name))
