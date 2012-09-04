(defproject deuce "0.1.0-SNAPSHOT"
  :description "DEUCE - Deuce is Emacs under Clojure"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/"
            :distribution :repo}
  :url "http://www.gnu.org/software/emacs/"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [clojure-lanterna "0.9.2"]]
  :plugins [[lein-swank "1.4.4"]
            [lein-difftest "1.3.8"]]
  :resource-paths ["emacs/lisp"]
  :jar-exclusions [#".*\.elc"]
  :java-source-paths ["src"]
  :jvm-opts ["-Xss4m"]
  :main deuce.main)
