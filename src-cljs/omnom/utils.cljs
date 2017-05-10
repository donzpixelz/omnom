(ns omnom.utils
  (:require [clojure.string :refer [replace]]
            [cljs-http.client :as http]))

(defn- name2
  "Changes keyword to string but respects backslashes"
  [k]
  (if (keyword? k) (.substring (str k) 1) k))

(defn- field-title [title] (replace (name2 title) #"[-_]" " "))

(def http-methods {"get" http/get
                   "delete" http/delete
                   "post" http/post
                   "put" http/put
                   "patch" http/patch})
