(ns swhd.cache
  "Persistent caches, as EDN files: the archived SWHIDs (a SWHID is
  immutable, so a known:true answer never expires) and the provenance
  the archive found for them (SWHID -> qualified SWHID). Negative
  answers are not persisted."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.nio.file CopyOption Files StandardCopyOption)))

(defn- cache-file [name]
  (str (or (System/getenv "XDG_CACHE_HOME")
           (str (System/getenv "HOME") "/.cache"))
       "/swhd/" name))

(defn default-file [] (cache-file "known.edn"))

(defn default-provenance-file [] (cache-file "provenance.edn"))

(defn- parse-edn
  "The value `text` holds when `valid?`, nil otherwise."
  [text valid?]
  (let [v (try (edn/read-string text) (catch Exception _ nil))]
    (when (valid? v) v)))

(defn parse-known
  "The set of SWHID strings `text` holds, nil when it is not an EDN set."
  [text]
  (parse-edn text set?))

(defn parse-provenance
  "The map of SWHID strings `text` holds, nil when it is not an EDN map
  of strings to strings."
  [text]
  (parse-edn text #(and (map? %) (every? string? (mapcat identity %)))))

(defn- load!
  "What `parse` reads in `file`, nil when there is none. A file that
  cannot be read as such is set aside (`file`.broken): the cache starts
  empty, the archive will be asked again, nothing is lost."
  [file parse]
  (let [f (io/file file)]
    (when (.exists f)
      (or (parse (slurp f))
          (do (.renameTo f (io/file (str file ".broken"))) nil)))))

(defn init
  "Cache state backed by `file` (EDN set of SWHID strings)."
  [file]
  (atom {:file (io/file file) :known (or (load! file parse-known) #{})}))

(defn init-provenance
  "Provenance cache backed by `file` (EDN map of SWHID strings to the
  qualified SWHID strings the archive found); in memory when `file` is
  nil."
  [file]
  (atom {:file (some-> file io/file)
         :known (or (some-> file (load! parse-provenance)) {})}))

(defn known? [cache swhid-str]
  (contains? (:known @cache) swhid-str))

(defn provenance
  "The qualified SWHID `cache` holds for `swhid-str`, nil when none."
  [cache swhid-str]
  (get (:known @cache) swhid-str))

(defn- persist!
  "Write the current :known value to :file (if any), atomically. The
  lock keeps concurrent http-kit worker threads from sharing the same
  tmp file."
  [cache]
  (locking cache
    (let [{:keys [file known]} @cache]
      (when-let [tmp (some-> file (str ".tmp") io/file)]
        (io/make-parents tmp)
        (spit tmp (pr-str known))
        (Files/move (.toPath tmp) (.toPath ^java.io.File file)
                    (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                            StandardCopyOption/REPLACE_EXISTING]))))))

(defn add!
  "Add SWHID strings to the cache; persist it only when it actually grew."
  [cache swhids]
  (when (seq swhids)
    (let [[old new] (swap-vals! cache update :known into swhids)]
      ;; new is a superset of old: it grew iff it is larger
      (when (> (count (:known new)) (count (:known old)))
        (persist! cache))))
  cache)

(defn add-provenance!
  "Remember that the archive found `swhid-str` as `qualified`; persist
  the cache only when that is new."
  [cache swhid-str qualified]
  (let [[old _] (swap-vals! cache assoc-in [:known swhid-str] qualified)]
    (when (not= qualified (get-in old [:known swhid-str]))
      (persist! cache)))
  cache)
