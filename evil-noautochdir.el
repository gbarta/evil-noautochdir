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
