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

(defun set-lisp-keybindings (map)
  (evil-define-key 'normal map
    "(" 'sp-backward-up-sexp
    ")" 'sp-end-of-sexp
    (kbd "SPC w") 'sp-wrap-round
    (kbd "SPC i") 'sp-wrap-round
    (kbd "SPC @") 'sp-splice-sexp
    (kbd "M-l") 'sp-forward-slurp-sexp
    (kbd "M-h") 'sp-forward-barf-sexp
    (kbd "SPC o") 'sp-raise-sexp))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
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
    (set-lisp-keybindings map))

  (dolist (mode '(emacs-lisp-mode
                  lisp-mode
                  coalton-mode
                  lisp-interactive-mode))
    (sp-local-pair mode "(" ")" :post-handlers '(my/insert-spaces-around-parens)) )

  (defun my/insert-spaces-around-parens (&rest _ignored)
    "Insert spaces around parentheses only when necessary."
    (save-excursion
      (backward-char)
      ;; Check if there's no space before the opening parenthesis
      (unless (or (bolp) ; Beginning of line
                  (looking-back "[[:space:]\t\n\(]" 1)) ; Space, tab, or newline before point
        (insert " "))
      ;; Move back inside the parentheses
      (sp-forward-sexp)
      ;; Check if there's no space after the closing parenthesis
      (unless (or (eolp) ; End of line
                  (looking-at "[[:space:]\t\n\)]")) ; Space, tab, or newline after point
        (insert " ")))))

(use-package evil-smartparens
  :ensure t
  :config
  (add-hook 'smartparens-enabled-hook 'evil-smartparens-mode))

(use-package evil-terminal-cursor-changer
  :ensure t
  :config
  (evil-terminal-cursor-changer-activate))

(defvar *coalton-mode-path*
  "~/Programs/etc/coalton-mode/")

(use-package slime
  :commands (slime)
  :config
  (setq inferior-lisp-program "ros -L sbcl-bin run")
  (when (file-exists-p (expand-file-name "slime-coalton.el" *coalton-mode-path*))
    (setq slime-contribs '(slime-fancy slime-coalton))
    (slime-require :swank-coalton))

  (define-key evil-normal-state-map (kbd "M-.") 'slime-edit-definition)
  (define-key evil-normal-state-map (kbd "M-,") 'slime-pop-find-definition-stack))

(use-package coalton-mode
  :load-path *coalton-mode-path*
  :mode (("\\.coal\\'" . coalton-mode))
  :config
  (set-lisp-keybindings coalton-mode-map))

(use-package eglot
  :commands (eglot)
  :config
  (add-to-list 'eglot-server-programs
               '((coalton-mode) . ("localhost" 7887))))

(fset 'yes-or-no-p 'y-or-n-p)
