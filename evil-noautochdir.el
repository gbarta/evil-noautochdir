(defvar evil-cwd default-directory)

(defadvice evil-ex (around evil-ex-noautochdir () activate)
  (let ((default-directory evil-cwd))
    ;; This depends on subtle and surprising behaviour which
    ;; happens to work out exactly as we would like due to
    ;; dynamic scoping and buffer-local variables, but probably
    ;; only due to an implementation detail.
    ;; By overriding default-directory with a scoped variable,
    ;; all of the evil and minibuffer and completion code will
    ;; get the directory we have set to emilate noautochdir.
    ;; However, if emacs sets the variable e.g. as part of
    ;; find-file, it will change the real buffer-local value
    ;; and not this temporary value, keeping normal emacs
    ;; behaviour of default-directory!
    ad-do-it))

(evil-define-command evil-cd (count dir)
  "Change directory."
  :repeat nil
  (interactive "P<f>")
  (if dir
      (setq evil-cwd (expand-file-name dir)))
  (message evil-cwd))
(evil-ex-define-cmd "cd" 'evil-cd)

(evil-define-command evil-pwd ()
  "Print current directory."
  :repeat nil
  (message evil-cwd))
(evil-ex-define-cmd "pwd" 'evil-pwd)

(provide 'evil-noautochdir)
