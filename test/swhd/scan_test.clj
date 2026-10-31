(ns swhd.scan-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is testing]]
            [swhd.scan :as scan]
            [swhd.test-util :refer [temp-dir delete-recursively]]
            [swhid.core :as swhid]
            [swhid.hash :as h]))

(defn- lookup
  "Archival status of the single artifact at `path`: swhd.scan/status of
  its artifact."
  [path known-fn known?]
  (scan/status (scan/artifact path) known-fn known?))

(defn- make-fixture []
  (let [dir (temp-dir "swhd-scan-test")]
    (spit (io/file dir "a.txt") "aaa\n")
    (.mkdir (io/file dir "sub"))
    (spit (io/file dir "sub" "b.txt") "bbb\n")
    dir))

(defn- swhid-of [f]
  (swhid/swhid->string (h/path-swhid f)))

(defn- recording-known-fn
  "known-fn answering from `answers` (default false), recording batches."
  [calls answers]
  (fn [swhids]
    (swap! calls conj (set swhids))
    (into {} (map (fn [s] [s (boolean (get answers s))])) swhids)))

(deftest fully-archived-root
  (let [dir (make-fixture)]
    (try
      (testing "root dir known => single call, no file queries, 100%"
        (let [calls (atom [])
              root-swhid (swhid-of dir)
              nodes
              (scan/scan dir (recording-known-fn calls {root-swhid true})
                         (constantly false))]
          (is (= 1 (count @calls)))
          ;; the only call contains directories only
          (is (every? #(str/starts-with? % "swh:1:dir:") (first @calls)))
          (is (every? :archived (vals nodes)))
          (is (= 4 (count nodes)))
          (is (map? nodes))))
      (finally (delete-recursively dir)))))

(deftest nothing-archived
  (let [dir (make-fixture)]
    (try
      (let [calls (atom [])
            nodes
            (scan/scan dir (recording-known-fn calls {}) (constantly false))]
        (is (= 2 (count @calls)))          ; dirs then files
        (is (not-any? :archived (vals nodes)))
        (is (map? nodes))
        (is (sorted? nodes)))
      (finally (delete-recursively dir)))))

(deftest partial-archive
  (let [dir (make-fixture)]
    (try
      (testing "sub/ known => b.txt covered; a.txt individually known"
        (let [sub-swhid (swhid-of (io/file dir "sub"))
              a-swhid (swhid-of (io/file dir "a.txt"))
              calls (atom [])
              nodes
              (scan/scan dir
                         (recording-known-fn calls {sub-swhid true a-swhid true})
                         (constantly false))]
          (is (get-in nodes ["sub" :archived]))
          (is (get-in nodes ["sub/b.txt" :archived]))   ; couvert par sub/
          (is (get-in nodes ["a.txt" :archived]))
          (is (not (get-in nodes ["." :archived])))
          (is (= 4 (count nodes)))
          ;; b.txt jamais demandé à l'API : couvert par sub/
          (is (not-any? #(contains? % (swhid-of (io/file dir "sub" "b.txt"))) @calls))))
      (finally (delete-recursively dir)))))

(deftest cache-short-circuits
  (let [dir (make-fixture)]
    (try
      (testing "root known in cache => zero API call"
        (let [root-swhid (swhid-of dir)
              calls (atom [])
              nodes
              (scan/scan dir (recording-known-fn calls {}) #(= % root-swhid))]
          (is (empty? @calls))
          (is (every? :archived (vals nodes)))
          (is (= 4 (count nodes)))))
      (finally (delete-recursively dir)))))

(defn- git! [dir & args]
  (let [{:keys [exit err]} (apply sh/sh "git" "-C" (str dir) args)]
    (assert (zero? exit) err)))

(defn- make-repo-fixture
  "The fixture as a Git repository, every file tracked."
  []
  (let [dir (make-fixture)]
    (git! dir "init" "-q")
    (git! dir "add" "-A")
    dir))

(deftest repo-scan-tracked-only
  (let [dir (make-repo-fixture)
        no-junk {:exclude #(contains? #{".git" "junk" "untracked.txt"} (.getName %))}]
    (try
      (spit (io/file dir ".gitignore") "junk/\n")
      (git! dir "add" ".gitignore")
      (.mkdir (io/file dir "junk"))
      (spit (io/file dir "junk" "x") "x\n")
      (spit (io/file dir "untracked.txt") "u\n")
      (testing "ignored and untracked files are out of the scan"
        (let [nodes
              (scan/scan dir (recording-known-fn (atom []) {}) (constantly false))]
          (is (= #{"." ".gitignore" "a.txt" "sub" "sub/b.txt"} (set (keys nodes))))
          (is (= 5 (count nodes)))
          ;; the root SWHID is the index tree: the walk without the junk
          (is (= (swhid/swhid->string (h/path-swhid dir no-junk))
                 (get-in nodes ["." :swhid])))))
      (testing "a subdirectory scan is cut out of the repository tree"
        (let [nodes
              (scan/scan (io/file dir "sub") (recording-known-fn (atom []) {}) (constantly false))]
          (is (= #{"." "b.txt"} (set (keys nodes))))
          (is (= (swhid-of (io/file dir "sub")) (get-in nodes ["." :swhid])))))
      (testing "an untracked directory falls back to a walk"
        (let [nodes
              (scan/scan (io/file dir "junk") (recording-known-fn (atom []) {}) (constantly false))]
          (is (= #{"." "x"} (set (keys nodes))))))
      (testing "an untracked file is hashed as git would store it"
        (spit (io/file dir ".gitattributes") "* text=auto\n")
        (git! dir "add" ".gitattributes")
        (spit (io/file dir "crlf.txt") "a\r\nb\r\n")
        (is (= (swhid/swhid->string (h/content-swhid "a\nb\n"))
               (:swhid (lookup (io/file dir "crlf.txt") (recording-known-fn (atom []) {})
                               (constantly false))))))
      (testing "an untracked symbolic link is hashed as its text, not as its target"
        (let [lnk (io/file dir "lnk")]
          (java.nio.file.Files/createSymbolicLink (.toPath lnk) (.toPath (io/file "a.txt"))
                                                  (make-array java.nio.file.attribute.FileAttribute 0))
          (is (= (swhid/swhid->string (h/content-swhid "a.txt"))
                 (:swhid (lookup lnk (recording-known-fn (atom []) {}) (constantly false)))))
          (.delete lnk)))
      (testing "an untracked file is asked for itself only"
        (let [calls (atom [])
              u (io/file dir "untracked.txt")]
          (is (false? (:archived (lookup u (recording-known-fn calls {})
                                         (constantly false)))))
          (is (= [#{(swhid-of u)}] @calls))))
      (testing "a modified tracked file is hashed from the working tree"
        (spit (io/file dir "a.txt") "changed\n")
        (let [calls (atom [])
              a (io/file dir "a.txt")]
          (is (= (swhid-of a)
                 (:swhid (lookup a (recording-known-fn calls {}) (constantly false)))))
          (is (= (swhid-of a)
                 (get-in (scan/scan dir (recording-known-fn (atom []) {}) (constantly false))
                         ["a.txt" :swhid])))))
      (finally (delete-recursively dir)))))

(defn- repo-swhid-of [f]
  (swhid/swhid->string (h/path-swhid f scan/exclude-git)))

(deftest lookup-pruning
  (let [dir (make-repo-fixture)
        b (io/file dir "sub" "b.txt")
        b-swhid (repo-swhid-of b)
        sub-swhid (repo-swhid-of (io/file dir "sub"))
        root-swhid (repo-swhid-of dir)]
    (try
      (testing "cached ancestor => zero API call, the file joins the cache"
        (let [calls (atom [])]
          (is (= {:swhid b-swhid :archived true :newly-known #{b-swhid}}
                 (lookup b (recording-known-fn calls {}) #(= % root-swhid))))
          (is (empty? @calls))))
      (testing "cached file => nothing new"
        (let [calls (atom [])]
          (is (= {:swhid b-swhid :archived true :newly-known #{}}
                 (lookup b (recording-known-fn calls {}) #(= % b-swhid))))
          (is (empty? @calls))))
      (testing "nothing cached => one call for the file and its ancestors"
        (let [calls (atom [])]
          (is (= {:swhid b-swhid :archived true :newly-known #{b-swhid sub-swhid}}
                 (lookup b (recording-known-fn calls {sub-swhid true})
                         (constantly false))))
          (is (= [#{b-swhid sub-swhid root-swhid}] @calls))))
      (testing "nothing archived"
        (is (= {:swhid b-swhid :archived false :newly-known #{}}
               (lookup b (recording-known-fn (atom []) {}) (constantly false)))))
      (testing "the repo root has no ancestor and matches the scan"
        (let [calls (atom [])]
          (is (= root-swhid (:swhid (lookup dir (recording-known-fn calls {})
                                            (constantly false)))))
          (is (= [#{root-swhid}] @calls))))
      (finally (delete-recursively dir)))))

(deftest lookup-outside-repo
  (let [dir (make-fixture)
        b (io/file dir "sub" "b.txt")]
    (try
      (testing "no repository root => only the artifact itself is asked"
        (let [calls (atom [])]
          (is (false? (:archived (lookup b (recording-known-fn calls {})
                                         (constantly false)))))
          (is (= [#{(swhid-of b)}] @calls))))
      (finally (delete-recursively dir)))))

(deftest subtree-test
  (let [nodes {"." "d0" "a" "d1" "a/x" "c1" "ab" "c2" "a/b" "d2" "a/b/y" "c3"}]
    (is (= nodes (scan/subtree nodes ".")))
    (is (= {"." "d1" "x" "c1" "b" "d2" "b/y" "c3"} (scan/subtree nodes "a"))
        "\"ab\" is not under \"a\"")
    (is (= {"." "d2" "y" "c3"} (scan/subtree nodes "a/b")))
    (is (nil? (scan/subtree nodes "untracked")))))
