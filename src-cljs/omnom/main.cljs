(ns omnom.main
  (:require [omnom.core :as core]
            [cemerick.url :as url]))

(enable-console-print!)

(defn- by-id [id] (.getElementById js/document (name id)))

(if-let [api (get (:query (url/url (-> js/window .-location .-href))) "api")]
  (let [api-url (url/url api)
        port (if-let [p (:port api-url)] (str ":" p) "")
        host (str (:protocol api-url) "://" (:host api-url) port)]
    (set! (.-value (by-id "start-point")) api)
    (core/omnom api-url (by-id "flibble") host))
  (set! (.-innerHTML (by-id "flibble")) "Enter the starting point of your API and hit explore."))
