(defproject omnom "0.1.0"
  :description "Finally a usable HATEOAS client in a decent language"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.8.51" :scope "provided"]
                 [hiccup "1.0.5"]
                 [hiccups "0.3.0"]
                 [cheshire "5.6.1"]
                 [cljs-http "0.1.42"]]

  :clojurescript? true
  :jar-exclusions [#"\.swp|\.swo|\.DS_Store"]
  :plugins [[lein-cljsbuild "1.1.3"]]
  :hooks [leiningen.cljsbuild]
  :cljsbuild {
    :builds [{:source-paths ["src-cljs"]
              :compiler {:output-to "resources/public/main.js"
                         :output-dir "resources/public"
                         :optimizations :simple
                         :source-map "resources/public/main.js.map"}}]})
