(autoload 'css-mode "css-mode" nil t)
(eval-after-load "css-mode"
  '(progn
     (setq cssm-indent-function 'cssm-c-style-indenter)
     (setq css-indent-offset 2)))

(unless (package-installed-p 'less-css-mode)
  (package-install 'less-css-mode))
(autoload 'less-css-mode "less-css-mode" nil t)

(unless (package-installed-p 'scss-mode)
  (package-install 'scss-mode))
(autoload 'scss-mode "scss-mode" nil t)
(eval-after-load "scss-mode"
  '(progn
     (setq scss-compile-at-save nil)))
