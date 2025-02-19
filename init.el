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
  :custom ((evil-want-C-u-scroll t)
           (evil-undo-system 'undo-redo))
  :config
  (evil-mode 1)
  (define-key evil-motion-state-map (kbd "C-z") 'suspend-frame)
  (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line))

(use-package smartparens
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode coalton-mode slime-repl-mode) . smartparens-strict-mode)
  :config
  (smartparens-global-mode)
  (require 'smartparens-config)

  (dolist (map (list emacs-lisp-mode-map
                     lisp-mode-map))
    (set-lisp-keybindings map))

  (sp-with-modes '(emacs-lisp-mode
                   lisp-mode
                   coalton-mode
                   lisp-interactive-mode)
    (sp-local-pair "(" ")"
                   :post-handlers '(my/insert-spaces-around-parens)
                   :when '(sp-in-code-p))
    (sp-local-pair "\"" "\""
                   :post-handlers '(my/insert-spaces-around-parens))
    (sp-local-pair "'" nil :actions nil))

  (defun my/insert-spaces-around-parens (id action context)
    "Insert spaces around parentheses only when necessary."
    (when (and (equal action 'insert)
               (not (equal id ")"))
               (not (and (equal id "(")
                         (or (looking-back "'(" 2)
                             (and (or (derived-mode-p 'lisp-mode)
                                      (derived-mode-p 'coalton-mode))
                                  (looking-back "#[+\-](" 3))))))
      (save-excursion
        (backward-char)
        ;; Check if there's no space before the opening parenthesis
        (unless (or (bolp)
                    (looking-back "[[:space:]\t\n\(]" 1))
          (insert " "))
        (sp-forward-sexp)
        ;; Check if there's no space after the closing parenthesis
        (unless (or (eolp)
                    (looking-at "[[:space:]\t\n\)]"))
          (insert " "))))))

(use-package evil-smartparens
  :ensure t
  :config
  (add-hook 'smartparens-enabled-hook 'evil-smartparens-mode))

(use-package evil-terminal-cursor-changer
  :ensure t
  :config
  (evil-terminal-cursor-changer-activate))

(use-package corfu-terminal
  :ensure t
  :custom ((corfu-auto t)
           (corfu-auto-delay 0.5)
           (corfu-auto-prefix 1)
           (corfu-cycle t)
           (tab-always-indent 'complete))
  :config
  (corfu-terminal-mode +1)
  (global-corfu-mode +1))

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

(menu-bar-mode -1)
(global-display-line-numbers-mode)

(setq-default indent-tabs-mode nil
              tab-width 2)

(setq-default show-trailing-whitespace t)

(custom-set-faces
 '(hl-line ((t (:background "#151515")))))

(global-hl-line-mode 1)

(savehist-mode 1)
(setq savehist-save-minibuffer-history t
      savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring
        shell-command-history
        extended-command-history))
