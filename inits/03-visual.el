(show-paren-mode t)

(require-or-install 'whitespace)
(set-face-foreground 'whitespace-space "DarkGoldenrod1")
(set-face-background 'whitespace-space nil)
(set-face-bold-p 'whitespace-space t)
(set-face-foreground 'whitespace-tab "DarkOliveGreen1")
(set-face-background 'whitespace-tab nil)
(set-face-underline 'whitespace-tab t)
(setq whitespace-style '(face tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark ?\t [?\xBB ?\t])))

(global-whitespace-mode 1)

(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background "ForestGreen"))
    (t
     ()))
  nil :group 'font-lock-highlighting-faces)
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)

(loop for mode in '(undo-tree-mode auto-complete-mode whitespace-mode)
      do (assq-delete-all mode minor-mode-alist))

(column-number-mode t)

(setq eol-mnemonic-dos "(CRLF)"
      eol-mnemonic-mac "(CR)"
      eol-mnemonic-unix "(LF)")

(setq-default line-spacing 0.1)

(when window-system
  (server-start)
  (set-frame-parameter nil 'alpha 85)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (toggle-scroll-bar nil))

;;
;; Japanese font

(defun set-jp-font (font)
  (when (display-graphic-p)
    (set-fontset-font
     (frame-parameter nil 'font)
     'japanese-jisx0208
     `(,font . "iso10646-1"))))

(when (mac-os-p)
  (add-hook 'window-setup-hook
            (lambda ()
              (custom-set-faces
               '(default ((t (:height 120 :family "Menlo")))))
              (set-jp-font "Hiragino Maru Gothic Pro"))))
