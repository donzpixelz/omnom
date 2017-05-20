(ns omnom.protocol.barf
  (:require [clojure.string :refer [lower-case replace split upper-case]]
            [clojure.walk :refer [postwalk]]
            [cljs-http.client :as http]
            [omnom.protocol.hiccup :as h]
            [omnom.utils :as u]))

;; Private functions

(defn- format-embedded [embedded host]
  (mapv
    #(let [t (get-in % [:_links :self :title])
           x (get-in % [:_links :self :href])]
      (if x
        (-> % (dissoc :_links) (assoc :href (h/->Link x host "get" t)))
        (-> % (dissoc :_links))))
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
          (u/http-methods (lower-case (name k))) (lower-case (name k))
          (map? v)                               (find-methods v)
          (sequential? v)                        (mapv #(find-methods %) v)
          (u/http-methods (lower-case v))        (lower-case v)
          :else                                  nil)))
  (first (remove nil? (flatten (find-methods map)))))

(defn- format-links [links host]
  (set
    (for [[k v] (dissoc links :curies)]
      (let [[a b] (split (u/name2 k) #":")]
        (if b
          {(u/field-title b) (h/->Link (curie-link a (:href v) links) host (http-method links) (:title v))}
          {(u/field-title k) (h/->Link (:href v)                      host (http-method {k v}) (:title v))})))))

;; Barfing records

(defrecord JSONHal [media-type])

(defrecord Form [media-type])

(defrecord NoContent [media-type])

(defrecord Error [media-type])

(defprotocol Barf (barf [this p1 p2] "Media Type independent markup barfing"))

(extend-protocol Barf
  JSONHal
  (barf [_ json host]
    (let [tidied (postwalk #(if (map? %) (dissoc % :templated) %) json)
          title (get-in tidied [:_links :self :href])
          entity (dissoc tidied :_links :_embedded)
          links (dissoc (:_links tidied) :self)]
      [:div
        (h/hiccup (h/->H3LinkTitle title host))
        (h/hiccup entity)
        (for [[embed-title embed-xs] (:_embedded tidied)]
          (h/hiccup [(h/->H2Title (u/name2 embed-title))
                     (format-embedded embed-xs host)]))
        (when (seq (dissoc links :curies)) (h/hiccup (h/->H2Title "links")))
        (h/hiccup (format-links links host))]))

  Form
  (barf [_ analysis host]
    [:form
      [:div {:class "form-group"}
        [:label {:for "method"} "Method"]
        [:select {:id "method" :name "method" :class "form-control"}
                (for [{:keys [body method]} analysis]
                  [:option {:value (str method "@@@@" body)} (upper-case method)])]]
      [:div {:class "form-group"}
        [:label {:for "payload"} "Body"]
        [:textarea {:id "payload" :name "payload" :class "form-control" :rows 10} ""]]
      [:button {:type "submit" :id "send-btn" :class "btn btn-primary pull-right"} "Send"]
    [:img {:src "empty.gif" :onload
"var selector = document.getElementById('method');
var editor = CodeMirror.fromTextArea(document.getElementById('payload'), {
  matchBrackets:     true,
  autoCloseBrackets: true,
  mode:              'application/json',
  lineWrapping:      true
});

editor.getDoc().setValue(selector.value.split('@@@@')[1]);
selector.addEventListener('change', function(){
  editor.getDoc().setValue(selector.value.split('@@@@')[1]);
});

document.getElementById('send-btn').addEventListener('click', function(event) {
  document.getElementById('payload').value = editor.getDoc().getValue();
  omnom.main.post(event);
});"}]])

  NoContent
  (barf [_ json status]
    [:div
      [:div {:class "alert alert-success"}
            (str "No Content - a " status " was returned")]])

  Error
  (barf [_ json status]
    [:div
      [:div {:class "alert alert-danger"}
            (str "Ooops a " status " was returned")]
      (h/hiccup json)]))
