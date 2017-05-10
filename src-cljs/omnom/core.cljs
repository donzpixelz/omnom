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
        ys (if name (filter #(= (lower-case (:method %)) (lower-case name)) xs) xs)
        aug-req (first ys)
        clj (if (:body aug-req) (js->clj (parse (:body aug-req))) nil)
        param-base {:with-credentials? false}
        param-headers (if (:Authorization (:headers aug-req)) (assoc param-base :headers {"Authorization" (:Authorization (:headers aug-req))}) param-base)
        param-qparams (if (:query-params aug-req) (assoc param-headers :query-params (:query-params aug-req)) param-headers)
        param-payload (if clj (assoc param-qparams :json-params clj) param-qparams)
        m-fn (u/http-methods (:method aug-req))]
    (println "aug-req:" aug-req)
    (println "param-payload:" param-payload)
    ((fn [x y] (m-fn x y)) uri param-payload)))

;; Public API

(defn ^:export omnom
  [uri name el host]
  (go (let [[_ api] (split (:path (url/url uri)) #"/")
            analysis (:body (<! (slurp (str "http://localhost:3001/services/" api "/analysis"))))
            rsp (<! (augmented-slurp uri name analysis))
            ;; TODO: dispatch on media type here for barfing)
            mkup (cond
                   (= (:status rsp) 204)                          (b/barf (b/->NoContent "hal+json") (:body rsp) (:status rsp))
                   (get http/unexceptional-status? (:status rsp)) (b/barf (b/->JSONHal "hal+json") (:body rsp) host)
                   :else                                          (b/barf (b/->Error "hal+json") (:body rsp) (:status rsp)))]
        (set! (.-innerHTML el) (-> mkup hiccups/html)))))
