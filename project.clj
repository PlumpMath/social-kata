(defproject social-kata "0.1.0-SNAPSHOT"
  :description "Social media kata"
  :url "http://github.com/thomasmulvaney/social-kata"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [prismatic/schema "0.4.3"]
                 [clj-time "0.9.0"]
                 [compojure "1.3.4"]
                 [ring-middleware-format "0.5.0"]
                 [ring/ring-json "0.3.1"]
                 [ring/ring-defaults "0.1.5"]
                 [log4j "1.2.17"]
                 [org.clojure/tools.logging "0.3.1"]]
  :plugins [[lein-ring "0.8.13"]
            [ring/ring-json "0.3.1"]]
  :ring {:handler social-kata.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]
                        [matcha "0.1.0"]
                        [expectations "2.0.9"]]}})
