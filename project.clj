(defproject scheep "0.1.0-SNAPSHOT"
  :description "A Scheme interpreter implemented in Clojure"
  :url "http://github.com/dawiedotcom/scheep"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.blancas/kern "0.7.0"]
                 [org.clojure/core.unify "0.5.5"]]
  :main scheep.core)
