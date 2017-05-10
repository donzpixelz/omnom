(ns omnom.protocol.barf
  (:require [clojure.data :refer [diff]]
            [clojure.string :refer [blank? escape join lower-case replace split]]
            [clojure.walk :refer [postwalk]]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [goog.string :as gs]
            [goog.string.format]
            [cemerick.url :as url]
            [hiccups.runtime :as hiccupsrt]
            [omnom.protocol.hiccup :as h])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [hiccups.core :as hiccups]))

;; Barfing records

(defrecord JSONHal [media-type])

(defrecord NoContent [media-type])

(defrecord Error [media-type])

(defprotocol Barf (barf [this json host] "Media Type independent markup barfing"))

(defn- name2
  "Changes keyword to string but respects backslashes"
  [k]
  (if (keyword? k) (.substring (str k) 1) k))

(defn- field-title [title] (replace (name2 title) #"[-_]" " "))

(def http-methods {"get" http/get "delete" http/delete "post" http/post "put" http/put "patch" http/patch})

(defn- format-embedded [embedded host]
  (mapv
    #(let [t (get-in % [:_links :self :title])
           x (get-in % [:_links :self :href])]
      (if x
        (-> % (dissoc :_links) (assoc :href (h/->Link x host "get" t)))
        (-> % (dissoc :_links))))
    embedded))

(defn- curie-link [a b links]
  (let [href (get-in (filterv #(= (:name %) a) (:curies links)) [0 :href])]
    (replace href "{rel}" b)))

(defn- http-method
  "Walks map and checks if keys or values specify an other HTTP method to 'GET'"
  [map]
  (defn find-methods [m]
      (for [[k v] m]
        (cond
          (get http-methods (lower-case (name k))) (lower-case (name k))
          (map? v)                                 (find-methods v)
          (sequential? v)                          (mapv #(find-methods %) v)
          (get http-methods (lower-case v))        (lower-case v)
          :else                                    nil)))
  (if-let [method (first (remove nil? (flatten (find-methods map))))]
    method
    "get"))

(defn- format-links [links host]
  (set
    (for [[k v] (dissoc links :curies)]
      (let [[a b] (split (name2 k) #":")]
        (if b
          {(field-title b) (h/->Link (curie-link a (:href v) links) host (http-method links) (:title v))}
          {(field-title k) (h/->Link (:href v)                      host (http-method {k v}) (:title v))})))))

(extend-protocol Barf
  JSONHal
  (barf [_ json host]
    (let [tidied (postwalk #(if (map? %) (dissoc % :templated) %) json)
          title (get-in tidied [:_links :self :href])
          entity (dissoc tidied :_links :_embedded)
          links (dissoc (:_links tidied) :self)]
      [:div
        (h/hiccup (h/->H1LinkTitle title host))
        (h/hiccup entity)
        (for [[embed-title embed-xs] (:_embedded tidied)]
          (h/hiccup [(h/->H2Title (name2 embed-title))
                   (format-embedded embed-xs host)]))
        (when (seq (dissoc links :curies)) (h/hiccup (h/->H2Title "links")))
        (h/hiccup (format-links links host))]))

  NoContent
  (barf [_ json status]
    [:div
      [:div {:class "alert alert-success"} (str "No Content - a " status " was returned")]])

  Error
  (barf [_ json status]
    [:div
      [:div {:class "alert alert-danger"} (str "Ooops a " status " was returned")]
      (h/hiccup json)]))
