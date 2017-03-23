(ns omnom.main
  (:require [omnom.core :as core]
            [cemerick.url :as url]))

(enable-console-print!)

(defn- elementById [id] (.getElementById js/document (name id)))

(when-let [api (get (:query (url/url (-> js/window .-location .-href))) "api")]
  (do
    (set! (.-value (elementById "start-url")) api)
    (core/omnom api (elementById "flibble"))))
