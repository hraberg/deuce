(defproject deuce "0.1.0-SNAPSHOT"
  :description "DEUCE - Deuce is (not yet) Emacs under Clojure"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/"
            :distribution :repo}
  :url "http://www.gnu.org/software/emacs/"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clojure-lanterna "0.9.3"]
                 [com.taoensso/timbre "1.5.2"]]
  :plugins [[lein-swank "1.4.5"]
            [lein-difftest "2.0.0"]
            [lein-marginalia "0.7.1"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.trace "0.7.5"]
                                  [swank-clojure "1.4.3"]
                                  [org.clojure/tools.nrepl "0.2.2"]
                                  [clojure-complete "0.2.2"]]}}
  :resource-paths ["emacs/lisp" "resources"]
  :jar-exclusions [#".*\.elc" #"TUTORIAL\..*"
                   #"ChangeLog.*" #"Makefile.*" #"README" #"\.gitignore"]
  :java-source-paths ["src"]
  :jvm-opts ["-Xss4m"]
  :main deuce.main)
