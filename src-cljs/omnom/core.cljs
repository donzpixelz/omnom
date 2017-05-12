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

(defn build-analysis-path
  [uri]
  (let [{:keys [protocol host path]} (url/url uri)
        [_ api] (split path #"/")]
    (str protocol "://" host ":3001" "/services/" api "/analysis")))

(defn- parse [json] (.parse js/JSON json))

(defn- success? [status] (get http/unexceptional-status? status))

(defn- uri-match?
  [u1 u2]
  (let [[left right both] (diff (rest (split u1 #"/")) (rest (split u2 #"/")))]
    (and (= (count left) (count right)) (<= (- (count left) (count right)) 1))))

(defn- augmented-slurp
  [uri name aug-req submit-body]
  (let [clj (when (not (nil? submit-body)) (js->clj (parse submit-body)))
        base {:with-credentials? false}
        hdrs (if-let [x (:Authorization (:headers aug-req))]
               (assoc base :headers {"Authorization" x})
               base)
        qps (if-let [x (:query-params aug-req)]
              (assoc hdrs :query-params x)
              hdrs)
        payload (if clj (assoc qps :json-params clj) qps)
        m-fn (u/http-methods (:method aug-req))]
    (println "sending:" payload)
    ((fn [x y] (m-fn x y)) uri payload)))

;; Public API

(defn ^:export omnom
  [uri name el host submit-method submit-body]
  (go
    (let [apath (build-analysis-path uri)
          analysis (:body (<! (slurp apath)))
          flt-anal (filter #(and (uri-match? (url/url (:uri %)) uri)
                                 (or (nil? submit-method) (= (lower-case (:method %)) submit-method)))
                           analysis)
          methods (map #(lower-case (:method %)) flt-anal)
          num (count flt-anal)
          good? (fn [] (and (= num 1) (or (not (nil? submit-method)) (= (first methods) "get"))))
          mkup (cond
                 (= num 0) (b/barf (b/->Error "hal+json") (str apath ": Not found") 404)
                 (good?)   (let [{:keys [status body]} (<! (augmented-slurp uri name (first flt-anal) submit-body))]
                             (cond
                               (= status 204)    (b/barf (b/->NoContent "hal+json") body status)
                               (success? status) (b/barf (b/->JSONHal "hal+json") body host)
                               :else             (b/barf (b/->Error "hal+json") body status)))
                 :else     (b/barf (b/->Form "hal+json") flt-anal host))]
       (set! (.-innerHTML el) (-> mkup hiccups/html)))))
