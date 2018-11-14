(ns metabase.plugins
  "The Metabase 'plugins' system automatically adds JARs in the `./plugins/` directory (parallel to `metabase.jar`) to
  the classpath at runtime. This works great on Java 8, but never really worked properly on Java 9; as of 0.29.4 we're
  planning on telling people to just add extra external dependencies with `-cp` when using Java 9."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [dynapath.util :as dynapath]
            [metabase
             [config :as config]
             [util :as u]]
            [metabase.util.i18n :refer [trs]])
  (:import clojure.lang.DynamicClassLoader
           java.io.File
           java.net.URL))

;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                              'System' Classloader                                              |
;;; +----------------------------------------------------------------------------------------------------------------+

(defn- system-classloader
  ^DynamicClassLoader []
  (let [sysloader (ClassLoader/getSystemClassLoader)]
    (if-not (instance? DynamicClassLoader sysloader)
      (log/error (trs "Error: System classloader is not an instance of clojure.lang.DynamicClassLoader.")
                 (trs "Make sure you start Metabase with {0}"
                      "-Djava.system.class.loader=clojure.lang.DynamicClassLoader"))
      sysloader)))

(defn class-for-name ^Class [^String classname]
  (Class/forName classname (boolean :initialize) (system-classloader)))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                              Add JAR to Classpath                                              |
;;; +----------------------------------------------------------------------------------------------------------------+

;; NOCOMMIT
(defn- root-dynamic-classloader [^DynamicClassLoader classloader]
  (let [parent (.getParent classloader)]
    (if (instance? DynamicClassLoader parent)
      (recur parent)
      classloader)))

(defmulti ^:private add-jar-to-classpath!
  "Dynamically add a JAR file to the classpath."
  {:arglists '([file-or-url])}
  class)

(defmethod add-jar-to-classpath! File [^File jar-file]
  (add-jar-to-classpath! (io/as-url jar-file)))

;; NOCOMMIT
(defn- current-classloader ^DynamicClassLoader []
  (let [classloader (.getContextClassLoader (Thread/currentThread))]
    (when (instance? DynamicClassLoader classloader)
      classloader)))

;; NOCOMMIT
(defn- current-root-classloader ^DynamicClassLoader []
  (some-> (current-classloader) root-dynamic-classloader))

;; NOCOMMIT
(println "system-classloader (@ launch)" (system-classloader))
(println "context class loader (@ launch)" (current-classloader))
(println "root dynamic class loader (@ launch)" (current-root-classloader))

(defmethod add-jar-to-classpath! URL [^URL url]
  (when-let [sysloader (system-classloader)]
    (dynapath/add-classpath-url sysloader url)
    (log/info (trs "Added {0} to classpath" url))

    ;; NOCOMMIT
    (println "Classpath URLs:" (u/pprint-to-str (dynapath/classpath-urls (system-classloader)))) ; NOCOMMIT

    (println "current context class loader" (current-classloader)) ; NOCOMMIT
    (println "current root dynamic class loader" (current-root-classloader))

    (some->
     (current-root-classloader)
     (dynapath/add-classpath-url url))

    (println "Classpath URLs:" (u/pprint-to-str (some-> (current-root-classloader) dynapath/classpath-urls)))
    ))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                          Add JAR-in-JAR to Classpath                                           |
;;; +----------------------------------------------------------------------------------------------------------------+

(defn- metabase-saved-deps-dir
  "Return the directory to save dependencies (extracted from the uberjar or otherwise) in. By default, attempts to
  create a dir called `deps` in the same current working directory; if that cannot be done for one reason or another,
  uses the system temporary directory."
  ^File []
  (or
   (when-let [user-dir (some-> (System/getProperty "user.dir") io/file)]
     (let [driver-deps-dir (io/file (io/file (System/getProperty "user.dir") "deps"))]
       (when-not (.exists driver-deps-dir)
         (.mkdir driver-deps-dir))
       (when (.canRead driver-deps-dir)
         driver-deps-dir)))
   (io/file (System/getProperty "java.io.tmpdir"))))

(defn- save-jar-to-deps-dir
  "Save a JAR file with URL `resource` to the Metabase saved dependencies dir under `filename-to-save-as`. This was
  written under the assumption that `resource` would refer to something available locally (i.e. within the uberjar
  itself), but it seems like this would also work for downloading and saving dependencies from the Internet (i.e., we
  can use this in the future when we add a 'Metabase Plugins Store'.)"
  ^File [^String filename-to-save-as, ^URL resource]
  (let [out-file (io/file (metabase-saved-deps-dir) filename-to-save-as)]
    (when (.exists out-file)
      (.delete out-file))
    (with-open [is (io/input-stream resource)]
      (io/copy is out-file))
    out-file))

(defn- inside-jar?
  "Does URL `resource` point to a file saved within a JAR?"
  [^URL resource]
  (str/includes? (.getFile resource) ".jar!"))

;; It seems like if we replace `inside-jar?` with something that also recognize Internet URLs we could extend this and
;; download and add plugins, e.g. implement the long-discussed Metabase Plugin Store
(defn- add-resource-to-classpath!
  "Add resource (presumably a JAR) denoted by URL `resource` to classpath. If `resource` is contained within a JAR,
  extract that JAR into the Metabase saved deps dir first, saving it under `temp-filename`."
  [^String temp-filename, ^URL resource]
  (log/info (trs "Adding resource {0} to classpath..." resource))
  (add-jar-to-classpath!
   (if-not (inside-jar? resource)
     resource
     (save-jar-to-deps-dir temp-filename resource))))

(defn add-resource-with-name-to-classpath!
  "Add a resource (i.e., a file in the `./resources/` directory of the Metabase project, presumably a JAR) with
  `filename` to the classpath. When running via Leiningen, the files are added directly from the resources directory;
  when running via an uberjar, the JAR (or other file) is first extracted into a temporary directory and then added to
  the classpath."
  [^String filename]
  (add-resource-to-classpath!
   filename
   (or (io/resource filename)
       (throw (Exception. (str (trs "Cannot find resource {0}" filename)))))))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                                 load-plugins!                                                  |
;;; +----------------------------------------------------------------------------------------------------------------+

(defn- plugins-dir
  "The Metabase plugins directory. This defaults to `plugins/` in the same directory as `metabase.jar`, but can be
  configured via the env var `MB_PLUGINS_DIR`."
  ^File []
  (let [dir (io/file (or (config/config-str :mb-plugins-dir)
                         (str (System/getProperty "user.dir") "/plugins")))]
    (when (and (.isDirectory dir)
               (.canRead dir))
      dir)))

(defn- dynamically-add-jars!
  "Dynamically add any JARs in the `plugins-dir` to the classpath.
   This is used for things like custom plugins or the Oracle JDBC driver, which cannot be shipped alongside Metabase
  for licensing reasons."
  []
  (when-let [^File dir (plugins-dir)]
    (log/info (trs "Loading plugins in directory {0}..." dir))
    (doseq [^File file (.listFiles dir)
            :when (and (.isFile file)
                       (.canRead file)
                       (re-find #"\.jar$" (.getPath file)))]
      (log/info (u/format-color 'magenta (trs "Loading plugin {0}... {1}" file (u/emoji "🔌"))))
      (add-jar-to-classpath! file))))

(defn load-plugins!
  "Dynamically add JARs if we're running on Java 8. For Java 9+, where this doesn't work, just display a nice message
  instead."
  []
  (dynamically-add-jars!)
  ;; NOCOMMIT
  (println "MetabaseClassLoader.findClass(org.sqlite.JDBC) ->" (.findClass (system-classloader) "org.sqlite.JDBC")) ; NOCOMMIT
  (let [klass (Class/forName "org.sqlite.JDBC" (boolean :initialize) (system-classloader))]
    (println "(Class/forName (system classloader)):" klass)
    (println "(.newInstance klass):" (.newInstance klass)) ; NOCOMMIT
    (println "java.sql.DriverManager.getClassLoader() -> " (.getClassLoader java.sql.DriverManager)) ; NOCOMMIT
    (println "org,postgresql.Driver.getClassLoader() -> "(.getClassLoader (Class/forName "org.postgresql.Driver")))
    (when klass
      (println "register" klass "::" (java.sql.DriverManager/registerDriver (.newInstance klass))) ; NOCOMMIT
      ))
  (println "DRIVERS ::" (u/pprint-to-str 'yellow
                          (enumeration-seq (java.sql.DriverManager/getDrivers))))
  (assert (java.sql.DriverManager/getDriver "jdbc:sqlite:")))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                            Running Plugin Setup Fns                                            |
;;; +----------------------------------------------------------------------------------------------------------------+

(defn setup-plugins!
  "Look for any namespaces on the classpath with the pattern `metabase.*plugin-setup` For each matching namespace, load
  it. If the namespace has a function named `-init-plugin!`, call that function with no arguments.

  This is intended as a startup hook for Metabase plugins to run any startup logic that needs to be done. This
  function is normally called once in Metabase startup, after `load-plugins!` runs above (which simply adds JARs to
  the classpath in the `plugins` directory.)"
  []
  ;; find each namespace ending in `plugin-setup`
  (doseq [ns-symb @u/metabase-namespace-symbols
          :when   (re-find #"plugin-setup$" (name ns-symb))]
    ;; load the matching ns
    (log/info (u/format-color 'magenta "Loading plugin setup namespace %s... %s" (name ns-symb) (u/emoji "🔌")))
    (require ns-symb)
    ;; now look for a fn in that namespace called `-init-plugin!`. If it exists, call it
    (when-let [init-fn-var (ns-resolve ns-symb '-init-plugin!)]
      (log/info (u/format-color 'magenta "Running plugin init fn %s/-init-plugin!... %s" (name ns-symb) (u/emoji "🔌")))
      (init-fn-var))))
