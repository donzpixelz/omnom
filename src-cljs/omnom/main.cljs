(ns omnom.main
  (:require [omnom.core :as core]))

(enable-console-print!)

(defn- elementById [id] (.getElementById js/document (name id)))

(core/omnom "http://localhost:3000/ho/individuals/123" (elementById "flibble"))
