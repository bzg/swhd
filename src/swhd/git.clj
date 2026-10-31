(ns swhd.git
  "The repository on disk, through the git command: its remote, the
  upstream of its current branch, its history and the objects in it."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]))

(defn- run
  "Trimmed output of a git command in `path`, nil when it fails or is empty."
  [path & args]
  (let [{:keys [exit out]} (apply sh/sh "git" "-C" (str path) args)]
    (when (zero? exit) (not-empty (str/trim out)))))

(defn pick-remote
  "Name of the remote to use among the set `remotes`: `tracked` (the
  current branch's) when it is one of them, which \".\" (the repository
  itself) or a URL (as `gh pr checkout` sets) is not, else \"origin\",
  else the only one there is. nil when none fits."
  [tracked remotes]
  (or (remotes tracked)
      (remotes "origin")
      (when (= 1 (count remotes)) (first remotes))))

(defn remote-url
  "URL of the repository's remote (see `pick-remote`), nil without one."
  [path]
  (let [branch (run path "symbolic-ref" "--short" "-q" "HEAD")
        tracked (when branch
                  (run path "config" "--get" (str "branch." branch ".remote")))
        remotes (set (some-> (run path "remote") str/split-lines))]
    (some->> (pick-remote tracked remotes)
             (run path "remote" "get-url"))))

(defn upstream-commit
  "Hash of the commit the upstream of the repository's current branch
  points at, as of the last fetch: what its remote holds, whatever is
  committed but not pushed yet. nil without an upstream, or when it is
  a local branch."
  [path]
  (when (some-> (run path "rev-parse" "--symbolic-full-name" "@{upstream}")
                (str/starts-with? "refs/remotes/"))
    (run path "rev-parse" "--verify" "-q" "@{upstream}^{commit}")))

(defn object-at
  "Hash of the object at `rel` (\".\" for the root directory) in
  `commit` of the repository, of `type` (\"blob\" or \"tree\") when
  given; nil when it has none."
  [path commit rel & [type]]
  (when-let [hash (run path "rev-parse" "--verify" "-q"
                       (if (= rel ".") (str commit "^{tree}") (str commit ":" rel)))]
    (when (or (nil? type) (= type (run path "cat-file" "-t" hash)))
      hash)))

(defn rev-list
  "Hashes of HEAD's last `n` commits, most recent first, with the git
  rev-list `args` after HEAD."
  [path n & args]
  (some-> (apply run path "rev-list" (str "--max-count=" n) "HEAD" args)
          str/split-lines))

(defn last-change
  "Hash of the commit of HEAD's history that last changed `rel`, nil
  when none did."
  [path rel]
  (run path "--literal-pathspecs" "log" "-1" "--format=%H" "HEAD" "--" rel))
