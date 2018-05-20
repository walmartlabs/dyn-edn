;;; Copyright (c) [2018]-present, Wal-Mart Store, Inc.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License."

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

(deftest dyn-boolean
  (is (= {:boolean true}
         (read-edn {'PRODUCTION "true"}
                   "{:boolean #dyn/boolean #dyn/prop PRODUCTION}"))))

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
