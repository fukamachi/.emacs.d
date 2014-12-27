(cond
 ((file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
  (load (expand-file-name "~/quicklisp/slime-helper.el")))
 ((file-exists-p (expand-file-name "~/.roswell/impls/ALL/ALL/quicklisp/slime-helper.el"))
  (load (expand-file-name "~/.roswell/impls/ALL/ALL/quicklisp/slime-helper.el")))
 (T (require-or-install 'slime)))

(require 'slime-autoloads)

(setq slime-default-lisp 'sbcl)
(setq slime-lisp-implementations
      `((sbcl ("ros" "+R" "-l" "~/.sbclrc" "-Q" "run") :coding-system utf-8-unix)
        (ccl ("ccl") :coding-system utf-8-unix)))
(add-hook 'slime-mode-hook
          (lambda ()
            (unless (slime-connected-p)
              (save-excursion (slime)))
            (global-set-key (kbd "C-c s") 'slime-selector)
            (define-key slime-scratch-mode-map (kbd "C-n") 'slime-eval-print-last-expression)
            (define-key slime-scratch-mode-map (kbd "C-j") 'next-line)))
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (define-key slime-repl-mode-map (kbd "C-n") 'slime-repl-newline-and-indent)
            (define-key slime-repl-mode-map (kbd "C-j") 'next-line)
            (define-key slime-repl-mode-map (kbd "M-r") 'anything-for-files)))
(setq slime-autodoc-use-multiline-p t)

(setq slime-contribs
      '(slime-fancy slime-banner slime-indentation))
(slime-setup slime-contribs)

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (el-get 'sync 'slime-repl-ansi-color)
            (slime-setup '(slime-repl-ansi-color))))

(require-or-install 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
(modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
(modify-syntax-entry ?\} "){" lisp-mode-syntax-table)

(font-lock-add-keywords 'lisp-mode '(("\\(?:^\\|[^,]\\)\\(@\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-comment-face))))
(font-lock-add-keywords 'lisp-mode '(("\\(?:^\\|^,:]\\)\\(<\\(?:\\sw\\|\\s_\\)+>\\)" (1 font-lock-type-face))))

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map "[" 'paredit-open-bracket)
     (define-key paredit-mode-map "]" 'paredit-close-bracket)
     (define-key paredit-mode-map "(" 'paredit-open-parenthesis)
     (define-key paredit-mode-map ")" 'paredit-close-parenthesis)
     (define-key paredit-mode-map "{" 'paredit-open-curly)
     (define-key paredit-mode-map "}" 'paredit-close-curly)

     (global-set-key (kbd "M-l") 'paredit-forward-slurp-sexp)
     (global-set-key (kbd "M-h") 'paredit-forward-barf-sexp)
     (global-set-key (kbd "M-9") 'paredit-wrap-round)
     (define-key paredit-mode-map (kbd "C-j") 'next-line)
     (define-key paredit-mode-map (kbd "C-k") 'previous-line)))

(loop for mode in '(emacs-lisp-mode
                    lisp-mode
                    lisp-interaction-mode
                    scheme-mode)
      do (add-hook (intern (concat (symbol-name mode) "-hook"))
                   (lambda ()
                     (require-or-install 'paredit)
                     (paredit-mode t))))

(defun set-pretty-patterns (patterns)
  (loop for (glyph . pairs) in patterns do
        (loop for (regexp . major-modes) in pairs do
              (loop for major-mode in major-modes do
                    (let ((major-mode (intern (concat (symbol-name major-mode) "-mode")))
                          (n (if (string-match "\\\\([^?]" regexp) 1 0)))
                      (font-lock-add-keywords major-mode
                                              `((,regexp (0 (prog1 ()
                                                              (compose-region (match-beginning ,n)
                                                                              (match-end ,n)
                                                                              ,glyph)))))))))))

(set-pretty-patterns
 '((?λ ("\\<lambda\\>" lisp lisp-interaction emacs-lisp scheme))
   (?λ ("\\<function\\>" js2))))
