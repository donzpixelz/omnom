(ns omnom.core
  (:require [clojure.data :refer [diff]]
            [clojure.string :refer [lower-case split]]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [cemerick.url :as url]
            [hiccups.runtime :as hiccupsrt]
            [omnom.protocol.barf :as b]
            [omnom.utils :as u])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [hiccups.core :as hiccups]))

;; Private functions

(defn- slurp [uri] (http/get uri {:with-credentials? false}))

(defn- parse [json] (.parse js/JSON json))

(defn- uri-match?
  [u1 u2]
  (let [[left right both] (diff (rest (split u1 #"/")) (rest (split u2 #"/")))]
    (and (= (count left) (count right)) (<= (- (count left) (count right)) 1))))

(defn- augmented-slurp
  [uri name augmented-requests]
  (let [xs (filter #(uri-match? (url/url (:uri %)) uri) augmented-requests)
        ys (if name
             (filter #(= (lower-case (:method %)) (lower-case name)) xs)
             xs)
        aug-req (first ys)
        clj (when (:body aug-req) (js->clj (parse (:body aug-req))))
        base {:with-credentials? false}
        hdrs (if [x (:Authorization (:headers aug-req))]
               (assoc base :headers {"Authorization" x})
               base)
        qps (if-let [x (:query-params aug-req)]
              (assoc hdrs :query-params x)
              hdrs)
        payload (if clj (assoc qps :json-params clj) qps)
        m-fn (u/http-methods (:method aug-req))]
    (println "aug-req:" aug-req)
    (println "payload:" payload)
    ((fn [x y] (m-fn x y)) uri payload)))

;; Public API

(defn ^:export omnom
  [uri name el host]
  (go
    (let [c-url (url/url uri)
          [_ api] (split (:path c-url) #"/")
          ahost (str (:protocol c-url) "://" (:host c-url) ":3001")
          analysis (:body (<! (slurp (str ahost "/services/" api "/analysis"))))
          rsp (<! (augmented-slurp uri name analysis))
          status (:status rsp)
          body (:body rsp)
          success? (fn [s] (nil? (get http/unexceptional-status? s)))
          ;; TODO: dispatch on media type here for barfing)
          mkup (cond
                 (= status 204) (b/barf (b/->NoContent "hal+json") body status)
                 (success? s)   (b/barf (b/->JSONHal "hal+json") body host)
                 :else          (b/barf (b/->Error "hal+json") body status))]
       (set! (.-innerHTML el) (-> mkup hiccups/html)))))
