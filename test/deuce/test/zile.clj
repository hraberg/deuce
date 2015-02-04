(ns deuce.test.zile
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [deuce.emacs]
            [deuce.emacs.lread :as lread])
  (:use [clojure.test]
        [deuce.test.common])
  (:import [java.io File]))

(with-loadup)
(with-fresh-emacs)

(defn zile-test [f]
  (let [expected (slurp (s/replace f #".el$" ".output"))
        stub (constantly nil)
        actual (with-redefs-fn {(resolve 'deuce.emacs/save-buffer) stub
                                (resolve 'deuce.emacs/save-buffers-kill-emacs) stub}
                 (fn []
                   (emacs (setq vc-handled-backends ())
                          (setq pop-up-windows nil)
                          (find-file "zile/tests/test.input"))
                   (lread/load (.getAbsolutePath (io/file f)))
                   (emacs (buffer-string))))]
    (is (= expected actual) f)))

(deftest zile
  (doseq [:let [passing #{"insert-char.el"}]
          f (->> (io/file "zile/tests")
                 .listFiles
                 (filter #(re-find #".el$" (str %)))
                 (filter (comp passing #(.getName ^File %)))
                 (map #(.getAbsolutePath ^File %))
                 sort)]
    (zile-test f)))
