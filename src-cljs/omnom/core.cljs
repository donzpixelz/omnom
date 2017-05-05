(ns omnom.core
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

(defn- escape-html [s]
  (escape s {"&"  "&amp;" ">"  "&gt;" "<"  "&lt;" "\"" "&quot;"}))

(defn- slurp [uri] (http/get uri {:with-credentials? false}))

(defn- name2
  "Changes keyword to string but respects backslashes"
  [k]
  (if (keyword? k) (.substring (str k) 1) k))

(defn- field-title [title] (replace (name2 title) #"[-_]" " "))

(defn- parse [json] (.parse js/JSON json))

(defn- uri-match? [u1 u2]
  (let [[left right both] (diff (rest (split u1 #"/")) (rest (split u2 #"/")))]
    (if (or (not= (count left) (count right)) (> (- (count left) (count right)) 1))
      false
      true)))

;; Hiccup records

(defrecord H1LinkTitle [title host])

(defrecord H2Title [title])

(defrecord Link [title host name title-attr])

;; Barfing records

(defrecord JSONHal [media-type])

(defrecord NoContent [media-type])

(defrecord Error [media-type])

(def http-methods {"get" http/get "delete" http/delete "post" http/post "put" http/put "patch" http/patch})

(defn- augmented-slurp [uri name augmented-requests]
  (let [xs (filter #(uri-match? (url/url (:uri %)) uri) augmented-requests)
        ys (if name (filter #(= (lower-case (:method %)) (lower-case name)) xs) xs)
        aug-req (first ys)
        clj (if (:body aug-req) (js->clj (parse (:body aug-req))) nil)
        param-base {:with-credentials? false}
        param-headers (if (:Authorization (:headers aug-req)) (assoc param-base :headers {"Authorization" (:Authorization (:headers aug-req))}) param-base)
        param-qparams (if (:query-params aug-req) (assoc param-headers :query-params (:query-params aug-req)) param-headers)
        param-payload (if clj (assoc param-qparams :json-params clj) param-qparams)
        m-fn (http-methods (:method aug-req))]
    (println "aug-req : " aug-req)
    (println "param-payload : " param-payload)
    ((fn [x y] (m-fn x y)) uri param-payload)))

(defn- format-embedded [embedded host]
  (mapv
    #(let [t (get-in % [:_links :self :title])
           x (get-in % [:_links :self :href])]
      (-> % (dissoc :_links) (assoc :href (->Link x host "get" t))))
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
          :else                                    Nil)))
  (if-let [method (first (remove nil? (flatten (find-methods map))))]
    method
    "get"))

(defn- format-links [links host]
  (set
    (for [[k v] (dissoc links :curies)]
      (let [[a b] (split (name2 k) #":")]
        (if b
          {(field-title b) (->Link (curie-link a (:href v) links) host (http-method links) (:title v))}
          {(field-title k) (->Link (:href v)                      host (http-method {k v}) (:title v))})))))

(defn create-link [host path]
  (url/url-encode
    (if (and (not (nil? path)) (.startsWith path "/")) (str host path) path)))

(defprotocol Hiccup (hiccup [this] "Hiccup markup"))

(defn- includes? [xs x] (not= -1 (.indexOf (str xs) x)))

(defn- barf-number [x] (if (includes? x ".") (gs/format "%.2f" x) (str x)))

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
      [:table {:class "pure-table"}
        [:tbody (for [[k v] this]
                  ^{:key k}[:tr [:th (hiccup (field-title k))]
                                [:td (hiccup v)]])]]))

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
          (hiccup [(->H2Title (name2 embed-title))
                   (format-embedded embed-xs host)]))
        (hiccup (->H2Title "links"))
        (hiccup (format-links links host))]))

  NoContent
  (barf [_ json status]
    [:div
      [:div {:class "success"} (str "No Content - a " status " was returned")]])

  Error
  (barf [_ json status]
    [:div
      [:div {:class "error"} (str "Ooops a " status " was returned")]
      (hiccup json)]))

(defn ^:export omnom [uri name el host]
  (go (let [[_ api] (split (:path (url/url uri)) #"/")
            analysis (:body (<! (slurp (str "http://localhost:3001/services/" api "/analysis"))))
            rsp (<! (augmented-slurp uri name analysis))
            ;; TODO: dispatch on media type here for barfing
            mkup (cond
                   (= (:status rsp) 204) (barf (->NoContent "hal+json") (:body rsp) (:status rsp))
                   (get http/unexceptional-status? (:status rsp)) (barf (->JSONHal "hal+json") (:body rsp) host)
                   :else (barf (->Error "hal+json") (:body rsp) (:status rsp)))]
        (set! (.-innerHTML el) (-> mkup hiccups/html)))))
