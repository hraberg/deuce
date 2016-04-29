(ns deuce.test.zile
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [deuce.emacs]
            [deuce.emacs-lisp :as el]
            [deuce.emacs.buffer :as buffer]
            [deuce.emacs.editfns :as editfns]
            [deuce.emacs.lread :as lread])
  (:refer-clojure :exclude [newline])
  (:use [clojure.test]
        [deuce.test.common])
  (:import [java.io File]))

(with-loadup)
(with-fresh-emacs)

(defn zile-test [f]
  (let [expected (slurp (s/replace f #".el$" ".output"))
        input-file (io/file (s/replace f #".el$" ".input"))
        buffer-name (.getName input-file)
        input-file (if (.exists input-file)
                     (.getAbsolutePath input-file)
                     "zile/tests/test.input")
        stub (constantly nil)
        actual (with-redefs-fn {(resolve 'deuce.emacs/save-buffer) stub
                                (resolve 'deuce.emacs/save-buffers-kill-emacs) stub}
                 (fn []
                   (try
                     (el/setq vc-handled-backends ())
                     (el/setq pop-up-windows nil)
                     ((resolve 'deuce.emacs/find-file) input-file)
                     (deuce.emacs.buffer/rename-buffer buffer-name)
                     (lread/load (.getAbsolutePath (io/file f)))
                     (editfns/buffer-string)
                     (finally
                       (some-> (buffer/get-buffer buffer-name)
                               buffer/kill-buffer)))))]
    (is (= expected actual))))

(doseq [:let [passing #{"backward-word.el"
                        "beginning-of-line.el"
                        "copy-region-as-kill.el"
                        "copy-to-register.el"
                        "end-of-line.el"
                        "exchange-point-and-mark.el"
                        "forward-line,.el"
                        "goto-char.el"
                        "goto-line.el"
                        "insert-buffer.el"
                        "insert-char.el"
                        "just-one-space.el"
                        "kill-buffer.el"
                        "kill-word.el"
                        "mark-whole-buffer.el"
                        "mark-word.el"
                        "newline.el"
                        "next-line.el"
                        "open-line.el"
                        "search-forward.el"
                        "search-forward-regexp.el"
                        "switch-to-buffer.el"
                        "toggle-read-onl.el"
                        "transpose-chars.el"
                        "transpose-lines.el"}]
        ^File f (->> (io/file "zile/tests")
                     .listFiles
                     (filter #(re-find #".el$" (str %)))
                     sort)
        :let [test-symbol (symbol (s/replace (.getName f) #".el$" ""))]]
  (if (passing (.getName f))
    (eval `(deftest ~test-symbol
             (zile-test ~(.getAbsolutePath ^File f))))
    (ns-unmap *ns* test-symbol)))
