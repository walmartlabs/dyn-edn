(ns com.walmartlabs.dyn-edn-test-secret
  (:require
    [clojure.test :refer :all]
    [clojure.edn :as edn]
    [com.walmartlabs.dyn-edn :refer [env-readers]]
    [com.walmartlabs.dyn-edn-tests]
    [clojure.java.io :as io])
  (:import (clojure.lang ExceptionInfo)
           (java.nio.file Files Path)))

(def read-edn #'com.walmartlabs.dyn-edn-tests/read-edn)
(def get-secrets #'com.walmartlabs.dyn-edn/get-secrets)

(defn acquire-resource []
  (let
    [temp-path (Files/createTempDirectory "dckstest" (into-array java.nio.file.attribute.FileAttribute []))
     test-secrets
     {"SECRET1" "THIS IS SECRET1"
      "SECRET2" "THIS IS SECRET2"
      "SECRET3" "80"
      "SECRET4" "true"
      "SECRET5" "example.org"}]
    {:test-secret-dir-path
     temp-path
     :test-secrets
     test-secrets
     :test-secret-file-paths
     (reduce
       (fn[accum [secret-name secret-value]]
         (let
           [secret-file (Files/createFile
                          (.resolve temp-path ^String secret-name)
                          (into-array java.nio.file.attribute.FileAttribute []))]
           (with-open
             [out-secret (io/writer (.toString secret-file))]
             (spit out-secret secret-value))
           (assoc accum secret-name secret-file)))
       {}
       test-secrets)}))

(defn release-resource [resource]
  (let [{secrets :test-secrets file-paths :test-secret-file-paths dir-path :test-secret-dir-path} resource]
    (mapv
      (fn[[_ secret-path]]
        (Files/delete secret-path))
      file-paths)
    (Files/delete dir-path)))

(def ^:dynamic ^:private *resource* nil)

(defmacro with-resource
  "Acquires resource and binds it locally to
  symbol while executing body. Ensures resource
  is released after body completes. If called in
  a dynamic context in which *resource* is
  already bound, reuses the existing resource and
  does not release it.
  From: Stuart Sierra"
  [symbol & body]
  `(let [~symbol (or *resource*
                     (acquire-resource))]
     (try ~@body
          (finally
            (when-not *resource*
              (release-resource ~symbol))))))

(defn files-fixture[f]
  (with-resource
    r
    (binding [*resource* r]
      (f))))

(use-fixtures :once files-fixture)

(deftest names-sanity
  (with-resource
    r
    (is
      (=
        (keys (:test-secret-file-paths r))
        (keys (:test-secrets r))))))

(deftest vals-sanity
  (with-resource
    r
    (is
      (=
        (mapv
          (fn[file-path]
            (slurp (.toString file-path)))
          (vals (:test-secret-file-paths r)))
        (vals (:test-secrets r))))))

(deftest secrets
  (with-resource
    r
    (is
      (= (into {}
               (mapcat
                 (fn[[k v]][[k v] [(symbol k) v]])
                 (:test-secrets r)))
         (get-secrets (.toString (:test-secret-dir-path r)))))))

(deftest secret-1
  (with-resource
    r
    (is (= {:result "THIS IS SECRET1"}
           (read-edn
             (get-secrets (.toString (:test-secret-dir-path r)))
             "{:result #dyn/prop SECRET1}")))))

(deftest secret-2
  (with-resource
    r
    (is (= {:result "THIS IS SECRET2"}
           (read-edn
             (get-secrets (.toString (:test-secret-dir-path r)))
             "{:result #dyn/prop SECRET2}")))))

(deftest secret-3
  (with-resource
    r
    (is (= {:result 80}
           (read-edn
             (get-secrets (.toString (:test-secret-dir-path r)))
             "{:result #dyn/long #dyn/prop SECRET3}")))))

(deftest secret-4
  (with-resource
    r
    (is (= {:result true}
           (read-edn
             (get-secrets (.toString (:test-secret-dir-path r)))
             "{:result #dyn/boolean #dyn/prop SECRET4}")))))

(deftest secret-4A
  (with-resource
    r
    (is (= {:result true}
           (read-edn
             (get-secrets (.toString (:test-secret-dir-path r)))
             "{:result #dyn/boolean #dyn/prop \"SECRET4\"}")))))

(deftest secret-5
  (with-resource
    r
    (is (= {:url "http://example.org:80"}
           (read-edn
             (get-secrets (.toString (:test-secret-dir-path r)))
             "{:url #dyn/join [\"http://\"
                                     #dyn/prop SECRET5
                                     \":\"
                                     #dyn/prop SECRET3]}")))))

(deftest secret-6
  (with-resource
    r
    (is
      (thrown-with-msg?
        clojure.lang.ExceptionInfo
        #"Dynamic property SECRET6 not found"
        (read-edn
          (get-secrets (.toString (:test-secret-dir-path r)))
          "{:result #dyn/prop SECRET6}")))))

