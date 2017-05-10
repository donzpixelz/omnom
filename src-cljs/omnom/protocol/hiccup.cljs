(ns omnom.protocol.hiccup
  (:require [clojure.data :refer [diff]]
            [clojure.string :refer [blank? escape join lower-case replace split]]
            [clojure.walk :refer [postwalk]]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [goog.string :as gs]
            [goog.string.format]
            [cemerick.url :as url]
            [hiccups.runtime :as hiccupsrt])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [hiccups.core :as hiccups]))

;; Hiccup records

(defrecord H1LinkTitle [title host])

(defrecord H2Title [title])

(defrecord Link [title host name title-attr])

(defprotocol Hiccup (hiccup [this] "Hiccup markup"))

(defn- escape-html [s]
  (escape s {"&"  "&amp;" ">"  "&gt;" "<"  "&lt;" "\"" "&quot;"}))

(defn- name2
  "Changes keyword to string but respects backslashes"
  [k]
  (if (keyword? k) (.substring (str k) 1) k))

(defn- field-title [title] (replace (name2 title) #"[-_]" " "))

(defn- includes? [xs x] (not= -1 (.indexOf (str xs) x)))

(defn- barf-number [x] (if (includes? x ".") (gs/format "%.2f" x) (str x)))

(defn create-link
  [host path]
  (url/url-encode
    (if (and (not (nil? path)) (.startsWith path "/")) (str host path) path)))


(extend-protocol Hiccup
  nil
  (hiccup [_] [:span nil])

  H1LinkTitle
  (hiccup
    [{:keys [title host]}]
    [:h1 [:a {:href (str "?api=" (create-link host title))} title]])

  H2Title
  (hiccup [this] [:h2 (:title this)])

  Link
  (hiccup
    [{:keys [title host name title-attr]}]
    [:a {:href (str "?api=" (create-link host title) "&name=" name) :title title-attr} title])

  js/Boolean
  (hiccup [this] [:span (str this)])

  js/Number
  (hiccup [this] [:span (barf-number this)])

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
      [:table {:class "table table-bordered"}
        [:tbody (for [[k v] this]
                  ^{:key k}[:tr [:th (hiccup (field-title k))]
                                [:td (hiccup v)]])]]))

  PersistentHashSet
  (hiccup [this]
    (if (empty? this)
      [:div [:span]]
      [:ul {:class "list-unstyled"}
        (for [item this] [:li (hiccup item)])]))

  PersistentVector
  (hiccup [this]
    (if (empty? this)
      [:div [:span]]
      [:ol {:class "list-unstyled"}
        (for [item this] [:li (hiccup item)])])))
