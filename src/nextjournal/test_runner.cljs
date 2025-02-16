(ns nextjournal.test-runner
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.test :as test]
            [clojure.tools.cli :as cli]
            [clojure.string :as str]
            [nbb.core :refer [await]]
            [promesa.core :as p])
  (:refer-clojure :exclude [test]))

(defn- ns-filter
  [{:keys [namespace namespace-regex]}]
  (let [[include-ns include-regexes]
        (if (or (seq namespace) (seq namespace-regex))
          [namespace namespace-regex]
          [nil [#".*\-test$"]])]
    (fn [ns]
      (or
        (get include-ns ns)
        (some #(re-matches % (name ns)) include-regexes)))))

(defn- var-filter
  [{:keys [var include exclude]}]
  (let [test-specific (if var
                        (set (map #(or (resolve %)
                                       (throw (ex-info (str "Could not resolve var: " %)
                                                       {:symbol %})))
                                  var))
                        (constantly true))
        test-inclusion (if include
                         #((apply some-fn include) (meta %))
                        (constantly true))
        test-exclusion (if exclude
                         #((complement (apply some-fn exclude)) (meta %))
                         (constantly true))]
    #(and (test-specific %)
          (test-inclusion %)
          (test-exclusion %))))

(defn- filter-vars!
  [nses filter-fn]
  (doseq [ns nses]
    (doseq [[_name var] (ns-publics ns)]
      (when (:test (meta var))
        (when (not (filter-fn var))
          (alter-meta! var #(-> %
                                (assoc ::test (:test %))
                                (dissoc :test))))))))

(defn- restore-vars!
  [nses]
  (doseq [ns nses]
    (doseq [[_name var] (ns-publics ns)]
      (when (::test (meta var))
        (alter-meta! var #(-> %
                              (assoc :test (::test %))
                              (dissoc ::test)))))))

(defn- contains-tests?
  "Check if a namespace contains some tests to be executed."
  [ns]
  (some (comp :test meta)
        (-> ns ns-publics vals)))

(defn file->ns [file]
  (let [first-form (read-string (str (fs/readFileSync file)))]
    (when (= 'ns (first first-form))
      (second first-form))))

#_(map file->ns (glob-cljs "test"))

(defn- get-dirs [path]
  (->> (fs/readdirSync path)
       (map #(path/join path %))
       (filter #(.isDirectory (fs/statSync %)))))

(defn- get-files [path]
  (->> (fs/readdirSync path)
       (map #(path/join path %))
       (filter #(.isFile (fs/statSync %)))))

(defn get-files-recursively [dir]
  (let [dirs (get-dirs dir)]
    (->> dirs
         (map get-files-recursively)
         (reduce concat)
         (concat (get-files dir)))))

(defn glob-cljs [dir]
  (filter #(or (str/ends-with? % ".cljs")
               (str/ends-with? % ".cljc")) 
          (get-files-recursively dir)))

(defn find-nses [dirs]
  (into (sorted-set)
        (comp (mapcat glob-cljs)
              (keep file->ns))
        dirs))

#_(find-nses #{"test" "src"})

(defn test
  [options]
  (let [dirs (or (:dir options)
                 #{"test"})
        nses (find-nses dirs)
        nses (filter (ns-filter options) nses)]
    (println (str "\nRunning tests in " dirs))
    (p/let [_ (apply require nses)]
      (try
        (filter-vars! nses (var-filter options))
        (apply test/run-tests (filter contains-tests? nses))
        (finally
          (restore-vars! nses))))))



(defn- parse-kw
  [^String s]
  (if (.startsWith s ":") (read-string s) (keyword s)))


(defn- accumulate [m k v]
  (update-in m [k] (fnil conj #{}) v))

(def cli-options
  [["-d" "--dir DIRNAME" "Name of the directory containing tests. Defaults to \"test\"."
    :parse-fn str
    :assoc-fn accumulate]
   ["-n" "--namespace SYMBOL" "Symbol indicating a specific namespace to test."
    :parse-fn symbol
    :assoc-fn accumulate]
   ["-r" "--namespace-regex REGEX" "Regex for namespaces to test."
    :parse-fn re-pattern
    :assoc-fn accumulate]
   ["-v" "--var SYMBOL" "Symbol indicating the fully qualified name of a specific test."
    :parse-fn symbol
    :assoc-fn accumulate]
   ["-i" "--include KEYWORD" "Run only tests that have this metadata keyword."
    :parse-fn parse-kw
    :assoc-fn accumulate]
   ["-e" "--exclude KEYWORD" "Exclude tests with this metadata keyword."
    :parse-fn parse-kw
    :assoc-fn accumulate]
   ["-H" "--test-help" "Display this help message"]])

(defn- help
  [args]
  (println "\nUSAGE:\n")
  (println "clj -m" (namespace `help) "<options>\n")
  (println (:summary args))
  (println "\nAll options may be repeated multiple times for a logical OR effect.")
  (println "If neither -n nor -r is supplied, use -r #\".*-test$\" (ns'es ending in '-test')"))

(defmethod test/report [:cljs.test/default :end-run-tests] [m]
  (when-not (test/successful? m)
    (set! (.-exitCode js/process) 1)))

(defn -main
  "Entry point for the test runner"
  [& args]
  (let [args (cli/parse-opts args cli-options)]
    (if (:errors args)
      (do (doseq [e (:errors args)]
            (println e))
          (help args)
          (js/process.exit 1))
      (if (-> args :options :test-help)
        (help args)
        (test (:options args))))))
