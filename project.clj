(defproject deuce "0.1.0-SNAPSHOT"
  :description "DEUCE - Deuce is Emacs under Clojure"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/"
            :distribution :repo}
  :url "http://www.gnu.org/software/emacs/"
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :resources-path "emacs/lisp"
  :jar-exclusions [#".*\.elc"]
  :java-source-path "src"
  :java-options ["-Xss32m"]
  :main deuce.core)
