(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

(autoload 'markdown-mode "markdown-mode" nil t)
(eval-after-load "markdown-mode"
  '(progn
     (define-key markdown-mode-map (kbd "C-=") 'markdown-cycle)
     (define-key markdown-mode-map (kbd "<tab>") 'ac-expand)))

(add-hook 'markdown-mode-hook
          (lambda ()
            (set (make-local-variable 'show-trailing-whitespace) nil)))
