(ns omnom.main
  (:require [clojure.string :refer [split]]
            [cemerick.url :as url]
            [form.juice]
            [omnom.core :as core]
            [omnom.macro :include-macros true :refer [project-version]]))

(enable-console-print!)

(defn- by-id [id] (.getElementById js/document (name id)))

(defn- do-stuff
  [submit-method submit-post]
  (let [u (url/url (-> js/window .-location .-href))]
    (if-let [api (get-in u [:query "api"])]
      (let [api-url (if (re-find #"\{\?" api) (url/url (first (split api #"\{\?"))) (url/url api))
            port (if-let [p (:port api-url)] (str ":" p) "")
            host (str (:protocol api-url) "://" (:host api-url) port)
            name (get-in u [:query "name"])]
        (set! (.-value (by-id "start-point")) api)
        (core/omnom api-url name (by-id "flibble") host submit-method submit-post))
      (set! (.-innerHTML (by-id "flibble")) "Enter the starting point of your API and hit explore."))))

(set! (.-innerHTML (by-id "version")) (str "v" (project-version)))

(do-stuff nil nil)

(defn post
  [event]
  (.preventDefault event) ; prevents default event of form submission to fire
  (let [{:keys [method payload]} (form.juice/squeeze event)
        [submit-method _] (split method #"@@@@")]
    (do-stuff submit-method payload)))
