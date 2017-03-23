(ns omnom.main
  (:require [omnom.core :as core]
            [cemerick.url :as url]))

(enable-console-print!)

(defn- elementById [id] (.getElementById js/document (name id)))

(let [base ""]
  (if-let [api (get (:query (url/url (-> js/window .-location .-href))) "api")]
    (core/omnom (str base api) (elementById "flibble"))
    (core/omnom (str base "http://localhost:9000/api/consumer/v/1/users/joe.bloggs@passiv.com") (elementById "flibble"))))
