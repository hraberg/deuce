(defproject deuce "0.1.0-SNAPSHOT"
  :description "DEUCE - Deuce is Emacs under Clojure"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/"
            :distribution :repo}
  :url "http://www.gnu.org/software/emacs/"
  :dependencies [[org.clojure/clojure "1.4.0-beta7"]]
  :dev-dependencies [[com.nativelibs4java/bridj "0.6"]]
  :repositories [["sonatype releases"
                  "https://oss.sonatype.org/content/repositories/releases/"]
                 ["nativelibs4java-repo"
                  "http://nativelibs4java.sourceforge.net/maven"]]
  :java-source-path "src"
  :javac-options ["-nowarn"]
  :extra-classpath-dirs ["lib-ext/*" "emacs/lisp"])
