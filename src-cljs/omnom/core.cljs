(ns omnom.core
  (:require [clojure.string :refer [blank? escape join replace]]
            [clojure.walk :refer [postwalk]]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http :refer [get]]
            [hiccups.runtime :as hiccupsrt])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [hiccups.core :as hiccups]))

(def uri-regex
  (re-pattern "(\\b(https?)://[-A-Za-z0-9+&@#/%?{}=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|{}])"))
  ;; TODO fix relative pattern (re-pattern "(\\b/[-A-Za-z0-9+&@#/%?{}=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|{}])"))

(defn- escape-html [s]
  (escape s {"&"  "&amp;" ">"  "&gt;" "<"  "&lt;" "\"" "&quot;"}))

(defn- slurp [uri] (get uri {:with-credentials? false :headers {"Authorization" "Bearer: xxxx"}}))

(defn- parse [json] (.parse js/JSON json))

(defn- clj [json] (js->clj (parse json) :keywordize-keys true))

(defn- intercept-links [uri] (replace uri uri-regex "<a href=$1 onclick=interceptLink(event);>$1</a>"))

(defn- format-embedded [embedded]
  (mapv #(let [x (get-in % [:_links :self :href])] (-> % (dissoc :_links) (assoc :href x))) embedded))

(defn- format-links [links] (set (for [[k v] links] {k (:href v)})))

(defrecord H1Title [title])

(defrecord H2Title [title])

(defprotocol Hiccup (hiccup [this] "Hiccup markup"))

(extend-protocol Hiccup
  nil
  (hiccup [_] [:span nil])

  H1Title
  (hiccup [this] [:h1 (:title this)])

  H2Title
  (hiccup [this] [:h2 (:title this)])

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

(defprotocol Barf (barf [this json] "Media Type independent markup barfing"))

(extend-protocol Barf
  JSONHal
  (barf [_ json]
    (let [tidied (postwalk #(if (map? %) (dissoc % :templated) %) json)
          title (get-in tidied [:_links :self :href])
          entity (dissoc tidied :_links :_embedded)
          links (dissoc (:_links tidied) :self)]
      [:div
        (hiccup (->H1Title title))
        (hiccup entity)
        (hiccup (->H2Title "links"))
        (for [[embed-title embed-xs] (:_embedded tidied)]
          (hiccup [(->H2Title embed-title) (format-embedded embed-xs)]))
        (hiccup (format-links links))])))

(defn ^:export omnom [uri el]
  (println "calling omnom")
  (println "uri : " uri)
  (println "el : " el)
  (go (let [rsp (<! (slurp uri))
            ;; TODO: dispatch on media type here for barfing
            mkup (barf (->JSONHal "hal+json") (:body rsp))]
        (set! (.-innerHTML el) (-> mkup hiccups/html intercept-links)))))
