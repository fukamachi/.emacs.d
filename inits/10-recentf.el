(require-or-install 'recentf-ext)
(setq recentf-max-saved-items 3000)
(setq recentf-exclude '("/TAGS$" "/var/tmp/"))
(global-set-key (kbd "M-r") 'anything-for-files)
(add-hook 'paredit-mode-hook
          (lambda ()
            (define-key paredit-mode-map (kbd "M-r") 'anything-for-files)))
