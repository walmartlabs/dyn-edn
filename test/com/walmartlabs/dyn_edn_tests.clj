(ns com.walmartlabs.dyn-edn-tests
  (:require
    [clojure.test :refer [deftest is are]]
    [clojure.edn :as edn]
    [com.walmartlabs.dyn-edn :refer [env-readers]])
  (:import (clojure.lang ExceptionInfo)))

(defn ^:private read-edn
  [properties string]
  (edn/read-string {:readers (env-readers properties)} string))

(deftest dyn-keyword
  (are [input output]
    (= {:kw output}
       (read-edn
         nil
         (str "{:kw #dyn/keyword " (pr-str input) "}")))

    "foo" :foo

    "foo/bar" :foo/bar))

(deftest dyn-long
  (is (= {:long 8080}
         (read-edn {'HTTP_PORT "8080"}
                   "{:long #dyn/long #dyn/prop HTTP_PORT}"))))

(deftest dyn-join
  (is (= {:url "http://example.org:8080/status"}
         (read-edn {'PORT "8080"
                    'HOST "example.org"
                    'PATH "/status"}
                   "{:url #dyn/join [\"http://\"
                                     #dyn/prop HOST
                                     \":\"
                                     #dyn/prop PORT
                                     #dyn/prop PATH]}"))))

(deftest dyn-prop
  ;; Each environment variable value is added under a string and a symbol key
  (is (= {:home-str (System/getenv "HOME")
          :home-sym (System/getenv "HOME")}
         (read-edn nil "{:home-str #dyn/prop \"HOME\"
                         :home-sym #dyn/prop HOME}")))

  (is (= {:default-a :a
          :default-b nil}
         (read-edn nil "{:default-a #dyn/prop [FLURB :a]
                         :default-b #dyn/prop [FLURB nil]}")))

  (is (thrown-with-msg? ExceptionInfo #"Dynamic property FLURB not found"
               (read-edn nil "{:fail #dyn/prop FLURB}"))))
