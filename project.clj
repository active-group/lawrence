(defproject lawrence "0.7.0-SNAPSHOT"
  :description "Lawrence: LR parser generator"
  :url "https://github.com/active-group/lawrence"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [active-clojure "0.3.0" :exclusions [org.clojure/clojure]]]
  :profiles {:dev {:dependencies [[org.clojure/tools.trace "0.7.8"]]}})
