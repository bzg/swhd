(ns swhd.origin-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [swhd.origin :as origin]))

(def fixtures
  "Shared with swh.el's tests: the two implementations must agree."
  (json/parse-string (slurp (io/resource "swhd/fixtures.json")) true))

(deftest normalize-test
  (testing "the archive's form of a remote URL, and whether it is public"
    (doseq [{:keys [url origin public]} (:normalize fixtures)]
      (is (= origin (origin/normalize url)) url)
      (is (= public (origin/public? (origin/normalize url))) url))))

(deftest pick-remote-test
  (is (= "up" (origin/pick-remote "up" #{"origin" "up"})) "the tracked one first")
  (is (= "origin" (origin/pick-remote "." #{"origin" "up"})) "\".\" is the repository itself")
  (is (= "origin" (origin/pick-remote nil #{"origin" "up"})))
  (is (= "up" (origin/pick-remote nil #{"up"})) "the only one there is")
  (is (nil? (origin/pick-remote nil #{"a" "b"})) "no way to choose")
  (is (nil? (origin/pick-remote nil #{}))))

(deftest fresh-test
  (let [entry {:value {:origin "u"} :at 1000}]
    (is (= {:origin "u"} (origin/fresh entry 1500 1000)))
    (is (nil? (origin/fresh entry 2000 1000)) "expired")
    (is (nil? (origin/fresh entry 1000 0)) "a ttl of 0 never keeps anything")
    (is (nil? (origin/fresh nil 1000 1000)) "no entry")))
