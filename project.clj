(defproject cchelper "1.0.0"
  :description "CharaChorder helper utility"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [com.github.kwhat/jnativehook "2.2.0"] ;; To capture keyboard events across OS
                 [seesaw "1.5.0"] ;; Wrapper over UI elements
                 [metosin/jsonista "0.3.4"] ;; Improved JSON handling
                 [bagotricks "1.5.5"] ;; My bag of tricks
                 ]
  :main ^:skip-aot cchelper.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
