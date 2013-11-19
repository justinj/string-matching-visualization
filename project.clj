(defproject strmatch "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [jayq "2.4.0"]
                 [org.clojure/clojurescript "0.0-2030"]]

  :profiles {:dev {:dependencies [[speclj "2.5.0"]
                                  [specljs "2.8.1"]]}}
  :plugins [[speclj "2.5.0"]
            [specljs "2.8.1"]
            [lein-cljsbuild "0.3.2"]]

  :cljsbuild ~(let [run-specs ["phantomjs" "bin/specljs_runner.js"  "public/javascript/strmatch_test.js"]]
          {:builds {
                    :dev {:source-paths ["src/cljs" "spec/cljs"]
                               :compiler {:output-to "public/javascript/strmatch_dev.js"
                                          :optimizations :whitespace
                                          :pretty-print true }}

                    :test {:source-paths ["src/cljs/strmatch/logic" "spec/cljs"]
                               :compiler {:output-to "public/javascript/strmatch_test.js"
                                          :optimizations :whitespace
                                          :pretty-print true}
                          :notify-command run-specs}

                    :prod {:source-paths ["src/cljs"]
                           :compiler {:output-to "public/javascript/strmatch.js"
                                      :optimizations :simple}}}

              :test-commands {"test" run-specs}})

  :source-paths ["src/clj" "src/cljs"]
  :test-paths ["spec/clj"])
