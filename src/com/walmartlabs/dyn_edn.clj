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
;;; limitations under the License.

(ns com.walmartlabs.dyn-edn
  "Support for #dyn/prop and related reader macros."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as str]))

(s/def ::non-blank-str (s/and string?
                              (complement str/blank?)))
(s/def ::any (constantly true))

(defn ^:private long-reader
  [v]
  (s/assert ::non-blank-str v)
  (Long/parseLong v))

(defn ^:private keyword-reader
  [v]
  (s/assert ::non-blank-str v)
  (if-let [slashx (str/index-of v \/)]
    (keyword (subs v 0 slashx) (subs v (inc slashx)))
    (keyword v)))

(defn ^:private boolean-reader
  [v]
  (s/assert ::non-blank-str v)
  (Boolean/parseBoolean v))

(s/def ::dyn-prop (s/or :simple ::key
                        :defaulted (s/tuple ::key ::any)))

(s/def ::key (s/or :string string?
                   :keyword keyword?
                   :symbol symbol?))

(defn ^:private dyn-prop-reader
  [properties]
  (let [missing-default (Object.)]
    (fn [v]
      (s/assert ::dyn-prop v)
      (let [[k default-value] (if (sequential? v)
                                v
                                [v missing-default])
            result (get properties k default-value)]
        (if-not (identical? missing-default result)
          result
          (throw (ex-info (format "Dynamic property %s not found" (pr-str k))
                          {:key k
                           :property-keys (keys properties)})))))))

(defn ^:private join-reader
  [v]
  (s/assert (s/coll-of (s/nilable string?)) v)
  (apply str v))

(defn readers
  "Creates a map of readers:

  * `#dyn/prop`
  * `#dyn/keyword`
  * `#dyn/long`
  * `#dyn/boolean`
  * `#dyn/join`

  The properties are used when creating the `#dyn/prop` reader."
  [properties]
  {'dyn/prop (dyn-prop-reader properties)
   'dyn/keyword keyword-reader
   'dyn/long long-reader
   'dyn/boolean boolean-reader
   'dyn/join join-reader})

(defn env-readers
  "Creates a properties map from environment variables and JVM system properties.

  Environment variables are added to the map twice: once with direct string keys,
  then again with the string key converted to a symbol.  This allows for bare
  symbols in the EDN to resolve.

  System properties are then added as key/value pairs; the keys are always strings.

  Finally, the provided properties are merged in and the result passed to [[readers]]."
  ([]
   (env-readers nil))
  ([properties]
   (let [env (->> (System/getenv)
                  (into {})
                  (reduce-kv
                    (fn [m k v]
                      (assoc m k v
                             (symbol k) v))
                    {}))]
     (readers
       (merge env
              (System/getProperties)
              properties)))))
