(unless (package-installed-p 'js2-mode)
  (package-install 'js2-mode))

(autoload 'js2-mode "js2" nil t)

(eval-after-load "js2-mode"
  '(progn
     (setq js2-basic-offset 2
           js2-mirror-mode nil)))

(unless (package-installed-p 'coffee-mode)
  (package-install 'coffee-mode))
(autoload 'coffee-mode "coffee-mode" nil t)
