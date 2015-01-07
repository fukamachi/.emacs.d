(add-hook 'cperl-mode-hook
          (lambda ()
            (font-lock-add-keywords nil '(("\\(throw\\|render\\)" 1 font-lock-keyword-face t)))))
