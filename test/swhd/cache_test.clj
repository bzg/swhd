(ns swhd.cache-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [swhd.cache :as cache]
            [swhd.test-util :refer [temp-dir delete-recursively]]))

(deftest cache-roundtrip
  (let [dir (temp-dir "swhd-cache-test")
        file (str (io/file dir "known.edn"))]
    (try
      (let [c (cache/init file)]
        (is (not (cache/known? c "swh:1:cnt:aa")))
        (cache/add! c ["swh:1:cnt:aa" "swh:1:dir:bb"])
        (is (cache/known? c "swh:1:cnt:aa")))
      (let [c2 (cache/init file)]
        (is (cache/known? c2 "swh:1:dir:bb"))
        (is (not (cache/known? c2 "swh:1:cnt:zz"))))
      (testing "re-adding known SWHIDs does not rewrite the file"
        (let [c (cache/init file)
              before (.lastModified (io/file file))]
          (cache/add! c ["swh:1:cnt:aa"])
          (is (= before (.lastModified (io/file file))))))
      (testing "a corrupt cache file is set aside, the cache starts empty"
        (spit file "{not a set")
        (let [c (cache/init file)]
          (is (not (cache/known? c "swh:1:cnt:aa")))
          (is (.exists (io/file (str file ".broken"))))
          (cache/add! c ["swh:1:cnt:aa"])
          (is (cache/known? (cache/init file) "swh:1:cnt:aa"))))
      (finally
        (delete-recursively dir)))))

(deftest provenance-cache-test
  (let [dir (temp-dir "swhd-cache-test")
        file (str (io/file dir "provenance.edn"))
        q "swh:1:cnt:aa;origin=https://example.org/lib;anchor=swh:1:rel:bb"]
    (try
      (let [c (cache/init-provenance file)]
        (is (nil? (cache/provenance c "swh:1:cnt:aa")))
        (cache/add-provenance! c "swh:1:cnt:aa" q)
        (is (= q (cache/provenance c "swh:1:cnt:aa"))))
      (is (= q (cache/provenance (cache/init-provenance file) "swh:1:cnt:aa"))
          "persisted")
      (testing "the same answer again does not rewrite the file"
        (let [before (.lastModified (io/file file))]
          (cache/add-provenance! (cache/init-provenance file) "swh:1:cnt:aa" q)
          (is (= before (.lastModified (io/file file))))))
      (testing "a corrupt file is set aside"
        (spit file "#{\"not a map\"}")
        (is (nil? (cache/provenance (cache/init-provenance file) "swh:1:cnt:aa")))
        (is (.exists (io/file (str file ".broken")))))
      (testing "without a file, in memory only"
        (let [c (cache/init-provenance nil)]
          (cache/add-provenance! c "swh:1:cnt:aa" q)
          (is (= q (cache/provenance c "swh:1:cnt:aa")))))
      (finally
        (delete-recursively dir)))))

(deftest parse-provenance-test
  (is (= {"swh:1:cnt:aa" "swh:1:cnt:aa;origin=x"}
         (cache/parse-provenance "{\"swh:1:cnt:aa\" \"swh:1:cnt:aa;origin=x\"}")))
  (is (nil? (cache/parse-provenance "{\"swh:1:cnt:aa\" 1}")) "not strings")
  (is (nil? (cache/parse-provenance "#{}")) "not a map"))

(deftest parse-known-test
  (is (= #{"swh:1:cnt:aa"} (cache/parse-known "#{\"swh:1:cnt:aa\"}")))
  (is (nil? (cache/parse-known "")) "an empty file")
  (is (nil? (cache/parse-known "[1 2]")) "not a set")
  (is (nil? (cache/parse-known "#{")) "truncated"))
