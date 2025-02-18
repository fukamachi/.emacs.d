(load (locate-user-emacs-file "functions.el"))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (add-hook 'evil-normal-state-entry-hook  'my-set-cursor-type-for-evil-state)
  (add-hook 'evil-insert-state-entry-hook  'my-set-cursor-type-for-evil-state)
  (add-hook 'evil-replace-state-entry-hook 'my-set-cursor-type-for-evil-state)
  (define-key evil-motion-state-map (kbd "C-z") 'suspend-frame))

(use-package smartparens
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode coalton-mode) . smartparens-strict-mode)
  :config
  (require 'smartparens-config)

  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-mode "'" nil :actions nil)
  (sp-local-pair 'coalton-mode "'" nil :actions nil)
  (dolist (map (list emacs-lisp-mode-map
                     lisp-mode-map))
    (set-lisp-keybindings map)))

(use-package evil-smartparens
  :ensure t
  :config
  (add-hook 'smartparens-enabled-hook 'evil-smartparens-mode))

(use-package slime
  :commands (slime)
  :config
  (setq inferior-lisp-program "ros -L sbcl-bin run")
  (setq slime-contribs '(slime-fancy slime-coalton))
  (slime-require :swank-coalton))

(use-package coalton-mode
  :load-path "~/Programs/etc/coalton-mode/"
  :mode (("\\.coal\\'" . coalton-mode))
  :config
  (set-lisp-keybindings coalton-mode-map))

(use-package eglot
  :commands (eglot)
  :config
  (add-to-list 'eglot-server-programs
               '((coalton-mode) . ("localhost" 7887))))

(fset 'yes-or-no-p 'y-or-n-p)
