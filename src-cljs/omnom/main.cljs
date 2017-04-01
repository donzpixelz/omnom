(ns omnom.main
  (:require [omnom.core :as core]
            [cemerick.url :as url]))

(enable-console-print!)

(defn- by-id [id] (.getElementById js/document (name id)))

(let [u (url/url (-> js/window .-location .-href))]
  (if-let [api (get-in u [:query "api"])]
    (let [api-url (url/url api)
          port (if-let [p (:port api-url)] (str ":" p) "")
          host (str (:protocol api-url) "://" (:host api-url) port)
          name (get-in u [:query "name"])]
      (set! (.-value (by-id "start-point")) api)
      (core/omnom api-url name (by-id "flibble") host))
    (set! (.-innerHTML (by-id "flibble")) "Enter the starting point of your API and hit explore.")))
