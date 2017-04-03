(ns omnom.main
  (:require [omnom.core :as core]
            [cemerick.url :as url]))

(enable-console-print!)

(defn- by-id [id] (.getElementById js/document (name id)))

(when-let [api (get (:query (url/url (-> js/window .-location .-href))) "api")]
  (set! (.-value (by-id "start-point")) api)
  (let [api-url (url/url api)
        port (if-let [p (:port api-url)] (str ":" p) "")
        host (str (:protocol api-url) "://" (:host api-url) port)
        name (get (:query (url/url (-> js/window .-location .-href))) "name")]
    (set! (.-value (by-id "start-point")) api)
    (core/omnom api-url name (by-id "flibble") host)))
