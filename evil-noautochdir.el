;;; evil-noautochdir.el --- equivalent to set noautochdir for evil

;; Copyright (C) 2014-2016 Gabriel Barta

;; Author: Gabriel Barta <gbarta@gabrielbarta.com>
;; Keywords: evil
;; Homepage: https://github.com/gbarta/evil-noautochdir
;; Created: 27th Feb 2015
;; Version: 0.2

;; The MIT License (MIT)
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This plugin provides an Evil implementation of the default Vim directory
;; behaviour, "set noautochdir". It only affects Evil ex commands while other
;; emacs functionality remains unaffected. It provides :pwd and :cd commands
;; to control the current directory.

;;; Code:

(require 'evil)

(defvar evil-cwd default-directory
  "Current working directory for issuing ex commands.")

(defmacro with-evil-cwd (&rest body)
  `(let ((default-directory evil-cwd))
     (progn ,@body)))

(defadvice evil-ex (around evil-ex-noautochdir () activate)
  (with-evil-cwd
   ;; This depends on subtle and surprising behaviour which
   ;; happens to work out exactly as we would like due to
   ;; dynamic scoping and buffer-local variables, but probably
   ;; only due to an implementation detail.
   ;; By overriding default-directory with a scoped variable,
   ;; all of the evil and minibuffer and completion code will
   ;; get the directory we have set to emulate noautochdir.
   ;; However, if emacs sets the variable e.g. as part of
   ;; find-file, it will change the real buffer-local value
   ;; and not this temporary value, keeping normal emacs
   ;; behaviour of default-directory!
   ad-do-it))

(evil-define-command evil-cd (dir)
  "Change directory."
  (interactive "<f>")
  (when (not dir)
    (setq dir "~"))
  (if (file-directory-p dir)
      (progn
        (setq evil-cwd (expand-file-name dir))
        (message evil-cwd))
    (error "Can't find directory %s in path" dir)))
(evil-ex-define-cmd "cd" 'evil-cd)

(defun evil-pwd ()
  "Print current directory."
  (interactive)
  (message evil-cwd))
(evil-ex-define-cmd "pwd" 'evil-pwd)


(provide 'evil-noautochdir)
;;; evil-noautochdir.el ends here
