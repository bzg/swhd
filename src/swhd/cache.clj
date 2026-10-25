(ns swhd.cache
  "Persistent cache of archived SWHIDs. A SWHID is immutable, so a
  known:true answer never expires; negative answers are not persisted."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.nio.file CopyOption Files StandardCopyOption)))

(defn default-file []
  (str (or (System/getenv "XDG_CACHE_HOME")
           (str (System/getenv "HOME") "/.cache"))
       "/swhd/known.edn"))

(defn parse-known
  "The set of SWHID strings `text` holds, nil when it is not an EDN set."
  [text]
  (let [v (try (edn/read-string text) (catch Exception _ nil))]
    (when (set? v) v)))

(defn init
  "Cache state backed by `file` (EDN set of SWHID strings). A file that
  cannot be read as such is set aside (`file`.broken) and the cache
  starts empty: the archive will be asked again, nothing is lost."
  [file]
  (let [f (io/file file)
        known (when (.exists f)
                (or (parse-known (slurp f))
                    (do (.renameTo f (io/file (str file ".broken"))) nil)))]
    (atom {:file f :known (or known #{})})))

(defn known? [cache swhid-str]
  (contains? (:known @cache) swhid-str))

(defn- persist!
  "Write the current :known set to :file, atomically. The lock keeps
  concurrent http-kit worker threads from sharing the same tmp file."
  [cache]
  (locking cache
    (let [{:keys [file known]} @cache
          tmp (io/file (str file ".tmp"))]
      (io/make-parents tmp)
      (spit tmp (pr-str known))
      (Files/move (.toPath tmp) (.toPath ^java.io.File file)
                  (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                          StandardCopyOption/REPLACE_EXISTING])))))

(defn add!
  "Add SWHID strings to the cache; persist it only when it actually grew."
  [cache swhids]
  (when (seq swhids)
    (let [[old new] (swap-vals! cache update :known into swhids)]
      ;; new is a superset of old: it grew iff it is larger
      (when (> (count (:known new)) (count (:known old)))
        (persist! cache))))
  cache)
