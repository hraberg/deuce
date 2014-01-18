(defproject deuce "0.1.0-SNAPSHOT"
  :description "DEUCE - Deuce is (not yet) Emacs under Clojure"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/"
            :distribution :repo}
  :url "http://www.gnu.org/software/emacs/"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clojure-lanterna "0.9.3"]
                 [com.taoensso/timbre "2.7.1"]
                 [org.tcrawley/dynapath "0.2.3"]
                 [fipp "0.4.1"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]]
  :repositories {"sonatype-staging"
                 "https://oss.sonatype.org/content/groups/staging/"}
  :plugins [[lein-difftest "2.0.0"]
            [lein-marginalia "0.7.1"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.trace "0.7.6"]
                                  [org.clojure/tools.nrepl "0.2.3"]]}
             :uberjar {:aot [deuce.main]}}
  :resource-paths ["emacs/lisp" "resources"]
  :jar-exclusions [#".*\.elc" #"TUTORIAL\..*"
                   #"ChangeLog.*" #"Makefile.*" #"README" #"\.gitignore"]
  :java-source-paths ["src"]
  :jvm-opts ["-Xss4m" "-XX:MaxPermSize=192m"]
  :main deuce.main)
