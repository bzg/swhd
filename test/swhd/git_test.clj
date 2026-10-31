(ns swhd.git-test
  (:require [clojure.test :refer [deftest is]]
            [swhd.git :as git]))

(deftest pick-remote-test
  (is (= "up" (git/pick-remote "up" #{"origin" "up"})) "the tracked one first")
  (is (= "origin" (git/pick-remote "." #{"origin" "up"})) "\".\" is the repository itself")
  (is (= "origin" (git/pick-remote "https://example.org/fork" #{"origin"})) "a URL is no remote")
  (is (= "origin" (git/pick-remote nil #{"origin" "up"})))
  (is (= "up" (git/pick-remote nil #{"up"})) "the only one there is")
  (is (nil? (git/pick-remote nil #{"a" "b"})) "no way to choose")
  (is (nil? (git/pick-remote nil #{}))))
