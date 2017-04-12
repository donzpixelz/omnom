(defproject omnom "0.1.0-alpha.4-SNAPSHOT"
  :description "Finally a usable HATEOAS client in a decent language"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.229" :scope "provided"]
                 [hiccup "1.0.5"]
                 [hiccups "0.3.0"]
                 [cheshire "5.6.1"]
                 [cljs-http "0.1.42"]
                 [com.cemerick/url "0.1.1"]]
  :clojurescript? true
  :jar-exclusions [#"\.swp|\.swo|\.DS_Store"]
  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-figwheel "0.5.10"]]
  :hooks [leiningen.cljsbuild]
  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src/cljs"]
              :figwheel true
              :compiler {:main "omnom.main"
                         :asset-path "js/out"
                         :output-to "resources/public/js/main.js"
                         :output-dir "resources/public/js/out"
                         :optimizations :none}}
             {:id "prod"
              :source-paths ["src/cljs"]
              :compiler {:output-to "resources/public/js/prod/main.js"
                         :output-dir "resources/public/js/prod"
                         :optimizations :simple
                         :source-map "resources/public/js/prod/main.js.map"}}]})
