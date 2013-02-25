(defproject deuce "0.1.0-SNAPSHOT"
  :description "DEUCE - Deuce is (not yet) Emacs under Clojure"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/"
            :distribution :repo}
  :url "http://www.gnu.org/software/emacs/"
  :dependencies [[org.clojure/clojure "1.5.0-RC17"]
                 [clojure-lanterna "0.9.2"]
                 [com.taoensso/timbre "1.3.0"]]
  :plugins [[lein-swank "1.4.5"]
            [lein-difftest "2.0.0"]
            [lein-marginalia "0.7.1"]]
  :profiles {:dev {:resource-paths ["emacs/test/automated"]}}
  :resource-paths ["emacs/lisp"]
  :jar-exclusions [#".*\.elc"]
  :java-source-paths ["src"]
  :jvm-opts ["-Xss4m"]
  :aot [deuce.emacs-lisp.error
        deuce.emacs-lisp.cons]
  :main deuce.main)
