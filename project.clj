(defproject deuce "0.1.0-SNAPSHOT"
  :description "DEUCE - Deuce is (not yet) Emacs under Clojure"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/"
            :distribution :repo}
  :url "http://www.gnu.org/software/emacs/"
  :dependencies [[org.clojure/clojure "1.7.0-alpha5"]
                 [clojure-lanterna "0.9.3"]
                 [com.taoensso/timbre "3.3.1"]
                 [org.tcrawley/dynapath "0.2.3"]
                 [fipp "0.5.1"]]
  :repositories {"sonatype-staging"
                 "https://oss.sonatype.org/content/groups/staging/"}
  :plugins [[lein-difftest "2.0.0"]
            [lein-marginalia "0.8.0"]]
  :profiles {:uberjar {:aot :all
                       :auto-clean false}
             :test {:global-vars {*warn-on-reflection* true}}}
  :resource-paths ["emacs/lisp" "resources"]
  :jar-exclusions [#".*\.elc" #"TUTORIAL\..*"
                   #"ChangeLog.*" #"Makefile.*" #"README" #"\.gitignore"]
  :jvm-opts ^:replace ["-Xss4m" "-Xmx1g"]
  :main deuce.main)
