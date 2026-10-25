(ns swhd.test-util
  "Helpers shared by the swhd test namespaces."
  (:require [clojure.java.io :as io])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (into-array FileAttribute []))))

(defn delete-recursively [f]
  (doseq [child (reverse (file-seq f))]
    (io/delete-file child true)))
