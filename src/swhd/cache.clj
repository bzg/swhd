(ns swhd.cache
  "Persistent caches, as append-only files of one entry per line: the
  archived SWHIDs (a SWHID is immutable, so a known:true answer never
  expires) and the provenance the archive found for them (SWHID, a tab,
  the qualified SWHID). A new entry is appended, the file is never
  rewritten: the cost of a write does not grow with the cache. Negative
  answers are not persisted."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io File FileOutputStream IOException RandomAccessFile)
           (java.nio.file CopyOption Files StandardCopyOption)))

(defn- cache-file [name]
  (str (or (System/getenv "XDG_CACHE_HOME")
           (str (System/getenv "HOME") "/.cache"))
       "/swhd/" name))

(defn default-file [] (cache-file "known.log"))

(defn default-provenance-file [] (cache-file "provenance.log"))

(defn- swhid? [s]
  (boolean (and s (re-matches #"swh:1:[a-z]{3}:[0-9a-f]+" s))))

(defn parse-known-line
  "The SWHID string a line of the known file holds, nil when it holds none."
  [line]
  (when (swhid? line) line))

(defn parse-provenance-line
  "The [SWHID qualified-SWHID] pair a line of the provenance file holds,
  nil when it holds none."
  [line]
  (let [[s q & more] (str/split line #"\t")]
    (when (and (swhid? s) (some? q) (str/starts-with? q "swh:1:") (nil? more))
      [s q])))

(defn- writable?
  "Whether the strings fit on a line of their own, a tab apart."
  [& ss]
  (not-any? #(re-find #"[\t\r\n]" %) ss))

(defn- partial-tail
  "Offset just after the last newline of `f` when a partial line follows
  it (a crash in the middle of a write), nil when `f` ends with a
  newline or is empty. Reads backwards, a chunk at a time."
  [^File f]
  (with-open [raf (RandomAccessFile. f "r")]
    (let [len (.length raf)]
      (when (and (pos? len)
                 (do (.seek raf (dec len)) (not= 10 (.read raf))))
        (loop [hi len]
          (let [lo (max 0 (- hi 65536))
                buf (byte-array (- hi lo))
                _ (do (.seek raf lo) (.readFully raf buf))
                nl (loop [i (dec (alength buf))]
                     (cond (neg? i) nil
                           (= 10 (aget buf i)) i
                           :else (recur (dec i))))]
            (cond nl (+ lo nl 1)
                  (zero? lo) 0
                  :else (recur lo))))))))

(defn- load!
  "`init` with the entries `parse-line` reads in `file`, if there is one.
  A partial last line is cut off, so that the next append starts on a
  line of its own; when the file cannot be written, it is just left
  out. Lines `parse-line` cannot read are skipped: the archive will be
  asked again."
  [file parse-line init log]
  (let [f (some-> file io/file)]
    (if (and f (.exists f))
      (let [cut (partial-tail f)
            cut? (when cut
                   (try (with-open [raf (RandomAccessFile. f "rw")]
                          (.setLength raf cut))
                        true
                        (catch IOException e
                          (log (str "cache: cannot repair " f ": " (ex-message e)))
                          false)))]
        (with-open [r (io/reader f)]
          (into init
                (keep parse-line)
                (cond-> (line-seq r) (and cut (not cut?)) butlast))))
      init)))

(defn- append!
  "Append `lines` to the cache's file, if it has one, in a single write.
  The lock keeps concurrent http-kit worker threads from interleaving.
  A failure (disk full, …) is logged, not thrown: the cache still holds
  the lines in memory, and the request that brought them must not fail."
  [cache lines]
  (let [{:keys [^File file log]} @cache]
    (when (and file (seq lines))
      (locking cache
        (try
          (io/make-parents file)
          (with-open [out (FileOutputStream. file true)]
            (.write out (.getBytes (str/join (map #(str % "\n") lines)) "UTF-8")))
          (catch IOException e
            (log (str "cache: cannot write " file ": " (ex-message e)))))))))

(defn init
  "Cache of archived SWHIDs backed by `file`, one SWHID per line. `log`,
  when given, receives a line about each file that cannot be written."
  ([file] (init file nil))
  ([file log]
   (let [log (or log (fn [_]))]
     (atom {:file (io/file file) :log log
            :known (load! file parse-known-line #{} log)}))))

(defn init-provenance
  "Provenance cache backed by `file`, one SWHID and the qualified SWHID
  the archive found for it per line (the last line about a SWHID
  wins); in memory when `file` is nil. `log` as for `init`."
  ([file] (init-provenance file nil))
  ([file log]
   (let [log (or log (fn [_]))]
     (atom {:file (some-> file io/file) :log log
            :known (load! file parse-provenance-line {} log)}))))

(defn known? [cache swhid-str]
  (contains? (:known @cache) swhid-str))

(defn provenance
  "The qualified SWHID `cache` holds for `swhid-str`, nil when none."
  [cache swhid-str]
  (get (:known @cache) swhid-str))

(defn add!
  "Add SWHID strings to the cache; append to its file those it did not hold."
  [cache swhids]
  (when (seq swhids)
    (let [[old _] (swap-vals! cache update :known into swhids)]
      ;; seq: distinct destructures its argument, which a set cannot be
      (append! cache (filter writable? (remove (:known old) (distinct (seq swhids)))))))
  cache)

(defn add-provenance!
  "Remember that the archive found `swhid-str` as `qualified`; append
  that to the cache's file when it is new."
  [cache swhid-str qualified]
  (let [[old _] (swap-vals! cache assoc-in [:known swhid-str] qualified)]
    (when (and (not= qualified (get-in old [:known swhid-str]))
               (writable? swhid-str qualified))
      (append! cache [(str swhid-str "\t" qualified)])))
  cache)

(defn migrate!
  "Turn `edn-file`, a cache of swhd 0.1 (an EDN collection), into the
  line file `file` unless that exists, with `entry->line` for each
  entry; then remove `edn-file`. An unreadable one is just removed."
  [edn-file file entry->line]
  (let [e (io/file edn-file)
        f (io/file file)]
    (when (.exists e)
      (when-not (.exists f)
        (let [v (try (edn/read-string (slurp e)) (catch Exception _ nil))
              lines (keep entry->line (when (coll? v) v))
              tmp (io/file (str f ".tmp"))]
          ;; all or nothing: a crash leaves the EDN file to convert again
          (io/make-parents tmp)
          (spit tmp (str/join (map #(str % "\n") lines)))
          (Files/move (.toPath tmp) (.toPath f)
                      (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))))
      (.delete e))))

(defn init-defaults
  "The daemon's caches, {:cache :provenance}, in their default files,
  where those of swhd 0.1 are migrated first; `log` as for `init`."
  [log]
  (migrate! (cache-file "known.edn") (default-file)
            #(when (and (string? %) (swhid? %) (writable? %)) %))
  (migrate! (cache-file "provenance.edn") (default-provenance-file)
            (fn [[s q]]
              (when (and (string? s) (string? q) (writable? s q))
                (str s "\t" q))))
  {:cache (init (default-file) log)
   :provenance (init-provenance (default-provenance-file) log)})
