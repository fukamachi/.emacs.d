(require-or-install 'tangotango-theme)
(load-theme 'tangotango t)

(defvar paren-face 'paren-face)
(make-face 'paren-face)
(set-face-foreground 'paren-face "#666666")

(dolist (mode '(lisp-mode
                emacs-lisp-mode
                scheme-mode
                clojure-mode))
  (font-lock-add-keywords mode
                          '(("(\\|)" . paren-face))))
