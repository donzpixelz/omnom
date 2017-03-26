(ns omnom.core
  (:require [clojure.string :refer [blank? escape join replace]]
            [clojure.walk :refer [postwalk]]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [hiccups.runtime :as hiccupsrt])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [hiccups.core :as hiccups]))

(defn- escape-html [s]
  (escape s {"&"  "&amp;" ">"  "&gt;" "<"  "&lt;" "\"" "&quot;"}))

(defn- slurp [uri] (http/get uri {:with-credentials? false}))

(defn- augmented-slurp [uri augmented-requests]
  (println "augmented-requests : " augmented-requests)
  (if-let [aug-req (first augmented-requests)] ;; TODO: currently handles only one augemted request
    (let [clj (js->clj (parse (:body aug-req)))]
      (println "clj : " clj)
      (println "uri : " uri)
      (println "actual extracted uri : " (:path uri))
      (println "augmented uri : " (:uri aug-req))
      (if (re-seq #"match" (:path uri))
        (http/post (:uri aug-req) {:json-params clj :with-credentials? false})
        (slurp uri))) ;; hackety hack adding trailing slash
    (slurp uri)))

(defn- parse [json] (.parse js/JSON json))

(defn- clj [json] (js->clj (parse json) :keywordize-keys true))

(defn- format-embedded [embedded host]
  (mapv
    #(let [x (get-in % [:_links :self :href])]
      (-> % (dissoc :_links) (assoc :href (->Link x host))))
    embedded))

(defn- format-links [links host]
  (set (for [[k v] links] {k (->Link (:href v) host)})))

(defn create-link [host path]
  (if (and (not (nil? path)) (.startsWith path "/")) (str host path) path))

(defrecord H1LinkTitle [title host])

(defrecord H2Title [title])

(defrecord Link [title host])

(defprotocol Hiccup (hiccup [this] "Hiccup markup"))

(extend-protocol Hiccup
  nil
  (hiccup [_] [:span nil])

  H1LinkTitle
  (hiccup [{:keys [title host]}] [:h1 [:a {:href (str "?api=" (create-link host title))} title]])

  H2Title
  (hiccup [this] [:h2 (:title this)])

  Link
  (hiccup [{:keys [title host]}] [:a {:href (str "?api=" (create-link host title))} title])

  js/Boolean
  (hiccup [this] [:span (str this)])

  js/Number
  (hiccup [this] [:span this])

  js/String
  (hiccup [this] [:span (escape-html this)])

  js/Date
  (hiccup [this] [:span (.toString this)])

  Keyword
  (hiccup [this] [:span (name this)])

  PersistentArrayMap
  (hiccup [this]
    (if (empty? this)
      [:div [:span]]
      [:table {:class "pure-table"}
        [:tbody (for [[k v] this] ^{:key k}[:tr [:th (hiccup k)] [:td (hiccup v)]])]]))

  PersistentHashSet
  (hiccup [this]
    (if (empty? this)
      [:div [:span]]
      [:ul (for [item this] [:li (hiccup item)])]))

  PersistentVector
  (hiccup [this]
    (if (empty? this)
      [:div [:span]]
      [:ol
        (for [item this] [:li (hiccup item)])])))

(defrecord JSONHal [media-type])

(defprotocol Barf (barf [this json host] "Media Type independent markup barfing"))

(extend-protocol Barf
  JSONHal
  (barf [_ json host]
    (let [tidied (postwalk #(if (map? %) (dissoc % :templated) %) json)
          title (get-in tidied [:_links :self :href])
          entity (dissoc tidied :_links :_embedded)
          links (dissoc (:_links tidied) :self)]
      [:div
        (hiccup (->H1LinkTitle title host))
        (hiccup entity)
        (for [[embed-title embed-xs] (:_embedded tidied)]
          (hiccup [(->H2Title embed-title) (format-embedded embed-xs host)]))
        (hiccup (->H2Title "links"))
        (hiccup (format-links links host))])))

(defn ^:export omnom [uri el host]
  (go (let [analysis (:body (<! (slurp "http://localhost:3001/services/ho/analysis")))
            aug-req (filter #(:body %) analysis)
            rsp (<! (augmented-slurp uri aug-req))
            ;; TODO: dispatch on media type here for barfing
            mkup (barf (->JSONHal "hal+json") (:body rsp) host)]
        (println "analysis : " aug-req)
        (set! (.-innerHTML el) (-> mkup hiccups/html)))))
