(ns omnom.core
  (:require [clojure.data :refer [diff]]
            [clojure.string :refer [blank? escape join lower-case replace split]]
            [clojure.walk :refer [postwalk]]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [cemerick.url :as url]
            [hiccups.runtime :as hiccupsrt])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [hiccups.core :as hiccups]))

(defn- escape-html [s]
  (escape s {"&"  "&amp;" ">"  "&gt;" "<"  "&lt;" "\"" "&quot;"}))

(defn- slurp [uri] (http/get uri {:with-credentials? false}))

(defn- parse [json] (.parse js/JSON json))

(defn- uri-match? [u1 u2]
  (let [[left right both] (diff (rest (split u1 #"/")) (rest (split u2 #"/")))]
    (if (or (not= (count left) (count right)) (> (- (count left) (count right)) 1))
      false
      true)))

(defn- write?[x] (some #{x} #{"post" "put" "patch" "delete"}))

(defn- augmented-slurp [uri name augmented-requests]
  (println "name : " name)
  (println "augmented requests : " augmented-requests)
  (let [xs (filter #(uri-match? (url/url (:uri %)) uri) augmented-requests)
        ys (if name (filter #(= (lower-case (:method %)) (lower-case name)) xs) xs)]
    (println "xs : " xs)
    (println "filtered aug requests : " ys)
    (if-let [aug-req (first ys)]
      (if (some #{(lower-case (or name "na"))} #{"post" "put" "patch" "delete"})
        (let [clj (js->clj (parse (:body aug-req)))]
          (println "augmented request : " aug-req)
          (http/post (:uri aug-req) {:json-params clj :with-credentials? false :headers {"Authorization" "BearerXYZ"}})) ;; TODO: add header from aug
        (http/get uri {:with-credentials? false :headers {"Authorization" "BearerXYZ"}})
        )

      (slurp uri))))

(defn- format-embedded [embedded host]
  (mapv
    #(let [x (get-in % [:_links :self :href])]
      (-> % (dissoc :_links) (assoc :href (->Link x host))))
    embedded))

(defn- format-links [links host]
  (let [curies (:curies links)
        c-link (fn [a b]
                (let [href (get-in (filterv #(= (:name %) a) curies) [0 :href])]
                  (replace href "{rel}" b)))
        items (for [[k v] (dissoc links :curies)]
                (let [[a b] (split (name k) #":")]
                  (if b
                    {b (->Link (c-link a (:href v)) host a (:name v) (:title v))}
                    {k (->Link (:href v) host a (:name v) (:title v))})))]
   (set items)))

(defn create-link [host path]
  (url/url-encode
    (if (and (not (nil? path)) (.startsWith path "/")) (str host path) path)))

(defrecord H1LinkTitle [title host])

(defrecord H2Title [title])

(defrecord Link [title host rel name title-attr])

(defprotocol Hiccup (hiccup [this] "Hiccup markup"))

(extend-protocol Hiccup
  nil
  (hiccup [_] [:span nil])

  H1LinkTitle
  (hiccup [{:keys [title host]}] [:h1 [:a {:href (str "?api=" (create-link host title))} title]])

  H2Title
  (hiccup [this] [:h2 (:title this)])

  Link
  (hiccup [{:keys [title host name title-attr]}] [:a {:href (str "?api=" (create-link host title) "&name=" name) :data-name name :title title-attr} title])

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

(defrecord Error [media-type])

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
          (hiccup [(->H2Title (.substring (str embed-title) 1)) ; (name :zones/1/properties) returns zones
                   (format-embedded embed-xs host)]))
        (hiccup (->H2Title "links"))
        (hiccup (format-links links host))]))

  Error
  (barf [_ json status]
    [:div
      [:div {:class "error"} (str "Ooops a " status " was returned")]
      (hiccup json)]))

(defn ^:export omnom [uri name el host]
  (go (let [analysis (:body (<! (slurp "http://localhost:3001/services/ho/analysis")))
            ;; aug-req (filter #(:body %) analysis)
            rsp (<! (augmented-slurp uri name analysis))
            ;; TODO: dispatch on media type here for barfing
            mkup (if (get http/unexceptional-status? (:status rsp))
                   (barf (->JSONHal "hal+json") (:body rsp) host)
                   (barf (->Error "hal+json") (:body rsp) (:status rsp)))]
        (set! (.-innerHTML el) (-> mkup hiccups/html)))))
