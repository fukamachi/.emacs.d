(require 'wdired)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)))

(require-or-install 'direx)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

(evil-declare-key 'normal direx:direx-mode-map (kbd "RET") 'direx:maybe-find-item)
