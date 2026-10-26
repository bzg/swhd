(ns swhd.scan
  "Scan a directory: SWHIDs from swhid-clj, archival status from a
  `known-fn`, with Merkle pruning -- directories are queried first, and
  everything under an archived directory is archived without further
  API calls. `lookup` applies the same pruning to a single artifact.

  Inside a Git repository, the SWHIDs are those of the tracked working
  tree (swhid.git/worktree-swhids): untracked and ignored files are left
  out, so directory SWHIDs are the ones the archive holds for a commit
  of this tree. Elsewhere, the directory is walked (\".git\" excluded).

  `known-fn` takes a coll of SWHID strings and returns {swhid boolean};
  `known?` is the cache lookup (swhid string -> boolean)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [swhid.core :as swhid]
            [swhid.git :as g]
            [swhid.hash :as h])
  (:import (java.nio.file Files LinkOption)))

(defn- ancestor-dirs
  "Ancestor directories of a relative path: \"a/b/c\" -> (\".\" \"a\" \"a/b\"),
  none for the root \".\"."
  [relpath]
  (when (not= relpath ".")
    (let [segs (str/split relpath #"/")]
      (cons "." (map #(str/join "/" (take % segs)) (range 1 (count segs)))))))

(defn positives
  "The keys of a {swhid boolean} map whose value is true."
  [known-map]
  (keep (fn [[s k]] (when k s)) known-map))

(def exclude-git
  "swhid.hash opts leaving \".git\" out of directory SWHIDs: the archive
  holds a repository's tree without it."
  {:exclude #(= ".git" (.getName %))})

(defn normalize
  "Absolute, normalized java.io.File for `path`."
  ^java.io.File [path]
  (-> (io/file path) .toPath .toAbsolutePath .normalize .toFile))

(defn repo-root
  "Nearest directory at or above `f` holding a \".git\" entry, or nil."
  [^java.io.File f]
  (->> (iterate #(.getParentFile ^java.io.File %)
                (if (.isDirectory f) f (.getParentFile f)))
       (take-while some?)
       (filter #(.exists (io/file % ".git")))
       first))

(defn relpath
  "\"/\"-separated path of `f` relative to `root`; \".\" for `root` itself."
  [^java.io.File root ^java.io.File f]
  (let [rel (-> (.relativize (.toPath root) (.toPath f))
                str
                (str/replace java.io.File/separator "/"))]
    (if (= rel "") "." rel)))

(defn- in-repo
  "For `f` inside a Git repository: {:nodes relpath -> SWHID string of
  the tracked working tree, :rel the path of `f` in it}; nil outside."
  [^java.io.File f]
  (when-let [repo (repo-root f)]
    {:repo repo
     :nodes (update-vals (g/worktree-swhids repo) swhid/swhid->string)
     :rel (relpath repo f)}))

(defn subtree
  "The entries of `nodes` (relpath -> SWHID string) at or under `rel`,
  with paths relative to it (\".\" for `rel` itself); nil when none."
  [nodes rel]
  (if (= rel ".")
    (not-empty nodes)
    (let [prefix (str rel "/")]
      (not-empty
       (into {}
             (keep (fn [[p s]]
                     (cond (= p rel) ["." s]
                           (str/starts-with? p prefix) [(subs p (count prefix)) s])))
             nodes)))))

(defn- tree-nodes
  "relpath -> SWHID string for the directory at `root` and everything
  under it (\".\" = root): from the Git index inside a repository (a
  subdirectory is cut out of the repository's tree), from a walk of the
  directory otherwise, or when the subdirectory is not tracked at all."
  [^java.io.File root]
  (or (when-let [{:keys [nodes rel]} (in-repo root)]
        (subtree nodes rel))
      (update-vals (h/tree-swhids root exclude-git) swhid/swhid->string)))

(defn scan
  "Scan the directory at `root`: a sorted map
    {relpath {:swhid s :dir? b :archived b}}   ; \".\" = root
  The 1/2 suffixes below mean: knowledge after that round of API calls."
  [root known-fn known?]
  (let [nodes (tree-nodes (normalize root))
        dir? (fn [p] (str/starts-with? (nodes p) "swh:1:dir:"))
        covered? (fn [known? p] (some #(known? (nodes %)) (ancestor-dirs p)))
        ;; the SWHIDs of the paths matching `pred` that `known?` neither
        ;; knows nor covers through an ancestor
        ask (fn [known? pred]
              (->> (keys nodes)
                   (filter pred)
                   (remove #(or (known? (nodes %)) (covered? known? %)))
                   (map nodes)
                   distinct))
        ;; Round 1: directories not covered by the cache.
        ask-dirs (ask known? dir?)
        dir-known (if (seq ask-dirs) (known-fn ask-dirs) {})
        known1? (fn [s] (or (known? s) (dir-known s)))
        ;; Round 2: files not covered by an archived directory.
        ask-files (ask known1? (complement dir?))
        file-known (if (seq ask-files) (known-fn ask-files) {})
        known2? (fn [s] (or (known1? s) (file-known s)))
        archived? (fn [p] (boolean (or (known2? (nodes p)) (covered? known2? p))))]
    (into (sorted-map)
          (map (fn [[p s]]
                 [p {:swhid s :dir? (dir? p) :archived (archived? p)}]))
          nodes)))

(defn artifact
  "The artifact at `path`, before the archive is asked about it:
    {:swhid s :dir? b :repo f-or-nil :tracked? b :dirs [ancestor-swhid]}
  Inside a Git repository, a tracked artifact gets its SWHID from the
  tracked working tree, and :dirs the SWHIDs of its ancestor directories;
  an untracked file is hashed as `git add` would store it (clean
  filters), like a tracked one. An untracked directory, or an artifact
  outside a repository, is hashed as it is on disk. A symbolic link is
  not a directory: as Git stores it, its SWHID is that of its text."
  [path]
  (let [f (normalize path)
        {:keys [repo nodes rel]} (in-repo f)
        tracked? (contains? nodes rel)]
    {:swhid (cond tracked? (nodes rel)
                  (and repo (.isFile f)) (swhid/swhid->string (g/blob-swhid repo rel))
                  :else (swhid/swhid->string (h/path-swhid f exclude-git)))
     :dir? (Files/isDirectory (.toPath f) (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
     :repo repo
     :tracked? tracked?
     :dirs (when tracked? (map nodes (ancestor-dirs rel)))}))

(defn status
  "Archival status of the `artifact` (see `artifact`), pruned like `scan`:
  an archived ancestor directory covers a tracked artifact. An untracked
  artifact is only asked for itself. Returns:
    {:swhid s :archived b :newly-known #{swhid-str}}
  The API gets at most one call, for `s` and its ancestors together.
  Once found archived, `s` is :newly-known even when only an ancestor
  said so: the next lookup stops at the cache."
  [{s :swhid dirs :dirs} known-fn known?]
  (if (known? s)
    {:swhid s :archived true :newly-known #{}}
    (let [covered? (boolean (some known? dirs))
          found (if covered?
                  #{}
                  (set (positives (known-fn (distinct (cons s dirs))))))
          archived (or covered? (boolean (seq found)))]
      {:swhid s
       :archived archived
       :newly-known (if archived (conj found s) #{})})))

(defn lookup
  "Archival status of the single artifact at `path`: `status` of its
  `artifact`."
  [path known-fn known?]
  (status (artifact path) known-fn known?))
