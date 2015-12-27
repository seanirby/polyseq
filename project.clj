(defproject polyseq "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.2.374"]
                 [org.clojure/clojurescript "1.7.170"]
                 [org.omcljs/om "0.9.0"]
                 [cljsjs/react-dom "0.14.3-1"]
                 [cljsjs/react-dom-server "0.14.3-0"]]
  :plugins [[lein-cljsbuild "1.1.2"]
            [lein-figwheel "0.5.0-2"]]
  :clean-targets ^{:protect false} ["resources/public/js/out"
                                    "resources/public/js/main.js"
                                    :target-path]
  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src"]
                        :figwheel true
                        :compiler {:main polyseq.core
                                   :asset-path "js/out"
                                   :output-to "resources/public/js/main.js"
                                   :output-dir "resources/public/js/out"
                                   :verbose true}}]}
  :figwheel {:css-dirs ["resources/public/css"]
             :open-file-command "emacsclient"})
