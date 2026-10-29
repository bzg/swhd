(ns swhd.cache-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [swhd.cache :as cache]
            [swhd.test-util :refer [temp-dir delete-recursively]]))

(deftest cache-roundtrip
  (let [dir (temp-dir "swhd-cache-test")
        file (str (io/file dir "known.log"))]
    (try
      (let [c (cache/init file)]
        (is (not (cache/known? c "swh:1:cnt:aa")))
        (cache/add! c ["swh:1:cnt:aa" "swh:1:dir:bb" "swh:1:cnt:aa"])
        (is (cache/known? c "swh:1:cnt:aa"))
        (is (= "swh:1:cnt:aa\nswh:1:dir:bb\n" (slurp file)) "one line each")
        (cache/add! c #{"swh:1:cnt:aa"})
        (is (= "swh:1:cnt:aa\nswh:1:dir:bb\n" (slurp file)) "a set, nothing new"))
      (let [c2 (cache/init file)]
        (is (cache/known? c2 "swh:1:dir:bb"))
        (is (not (cache/known? c2 "swh:1:cnt:zz"))))
      (testing "only what is new is appended"
        (let [c (cache/init file)]
          (cache/add! c ["swh:1:cnt:aa" "swh:1:cnt:cc"])
          (is (= "swh:1:cnt:aa\nswh:1:dir:bb\nswh:1:cnt:cc\n" (slurp file)))))
      (testing "a line cut by a crash is dropped, appends start afresh"
        (spit file "swh:1:cnt:dd\nswh:1:cnt:e" :append true)
        (let [c (cache/init file)]
          (is (cache/known? c "swh:1:cnt:dd"))
          (is (not (cache/known? c "swh:1:cnt:e")))
          (cache/add! c ["swh:1:cnt:ff"])
          (is (str/ends-with? (slurp file) "swh:1:cnt:dd\nswh:1:cnt:ff\n"))))
      (testing "unreadable lines are skipped"
        (spit file "garbage\n\nswh:1:cnt:aa trailing\n" :append true)
        (let [c (cache/init file)]
          (is (cache/known? c "swh:1:cnt:ff"))
          (is (= 5 (count (:known @c))))))
      (finally
        (delete-recursively dir)))))

(deftest cache-failures
  (let [dir (temp-dir "swhd-cache-test")
        file (io/file dir "known.log")
        lines (atom [])
        log #(swap! lines conj %)]
    (try
      (testing "a partial line longer than a read chunk is cut off"
        (spit file (str "swh:1:cnt:aa\n" (apply str (repeat 100000 "x"))))
        (is (= #{"swh:1:cnt:aa"} (:known @(cache/init file log))))
        (is (= "swh:1:cnt:aa\n" (slurp file))))
      (testing "a file without any newline is emptied"
        (spit file "swh:1:cnt:a")
        (is (= #{} (:known @(cache/init file log))))
        (is (= "" (slurp file))))
      (testing "a read-only file loads, without its partial line"
        (spit file "swh:1:cnt:aa\nswh:1:cnt:b")
        (.setWritable file false)
        (when-not (.canWrite file)          ; root writes anyway
          (let [c (cache/init file log)]
            (is (= #{"swh:1:cnt:aa"} (:known @c)))
            (is (re-find #"cannot repair" (first @lines)))
            (testing "and a write that fails is logged, not thrown"
              (cache/add! c ["swh:1:cnt:cc"])
              (is (cache/known? c "swh:1:cnt:cc") "kept in memory")
              (is (re-find #"cannot write" (last @lines))))))
        (.setWritable file true))
      (testing "a file that cannot be opened at all for writing"
        (let [c (cache/init (io/file dir "sub") log)]
          (.mkdir (io/file dir "sub"))
          (cache/add! c ["swh:1:cnt:dd"])
          (is (cache/known? c "swh:1:cnt:dd"))
          (is (re-find #"cannot write .*sub" (last @lines)))))
      (finally
        (delete-recursively dir)))))

(deftest provenance-cache-test
  (let [dir (temp-dir "swhd-cache-test")
        file (str (io/file dir "provenance.log"))
        q "swh:1:cnt:aa;origin=https://example.org/lib;anchor=swh:1:rel:bb"]
    (try
      (let [c (cache/init-provenance file)]
        (is (nil? (cache/provenance c "swh:1:cnt:aa")))
        (cache/add-provenance! c "swh:1:cnt:aa" q)
        (is (= q (cache/provenance c "swh:1:cnt:aa"))))
      (is (= q (cache/provenance (cache/init-provenance file) "swh:1:cnt:aa"))
          "persisted")
      (testing "the same answer again is not appended"
        (cache/add-provenance! (cache/init-provenance file) "swh:1:cnt:aa" q)
        (is (= (str "swh:1:cnt:aa\t" q "\n") (slurp file))))
      (testing "the last line about a SWHID wins"
        (cache/add-provenance! (cache/init-provenance file) "swh:1:cnt:aa" "swh:1:cnt:aa;origin=x")
        (is (= "swh:1:cnt:aa;origin=x"
               (cache/provenance (cache/init-provenance file) "swh:1:cnt:aa"))))
      (testing "what does not fit on a line stays in memory"
        (let [c (cache/init-provenance file)]
          (cache/add-provenance! c "swh:1:cnt:bb" "swh:1:cnt:bb;origin=a\nb")
          (is (= "swh:1:cnt:bb;origin=a\nb" (cache/provenance c "swh:1:cnt:bb")))
          (is (nil? (cache/provenance (cache/init-provenance file) "swh:1:cnt:bb")))))
      (testing "without a file, in memory only"
        (let [c (cache/init-provenance nil)]
          (cache/add-provenance! c "swh:1:cnt:aa" q)
          (is (= q (cache/provenance c "swh:1:cnt:aa")))))
      (finally
        (delete-recursively dir)))))

(deftest parse-lines-test
  (is (= "swh:1:cnt:aa" (cache/parse-known-line "swh:1:cnt:aa")))
  (is (nil? (cache/parse-known-line "")))
  (is (nil? (cache/parse-known-line "swh:1:cnt:aa;origin=x")) "a core SWHID only")
  (is (= ["swh:1:cnt:aa" "swh:1:cnt:aa;origin=x"]
         (cache/parse-provenance-line "swh:1:cnt:aa\tswh:1:cnt:aa;origin=x")))
  (is (nil? (cache/parse-provenance-line "swh:1:cnt:aa")) "no qualified SWHID")
  (is (nil? (cache/parse-provenance-line "swh:1:cnt:aa\tx\ty")) "too many fields"))
