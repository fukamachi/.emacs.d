(when (mac-os-p)
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super)
  (setq mac-pass-command-to-system nil))
(when (linuxp)
  (setq x-super-keysym 'meta)
  (setq x-meta-keysym 'super))

(define-key global-map (kbd "RET") 'newline-and-indent)

(defun font-big ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (+ (face-attribute 'default :height) 10)))

(defun font-small ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (- (face-attribute 'default :height) 10)))

(defvar maxframe-fullscreen-p nil)

(defun toggle-fullscreen ()
  (interactive)
  (cond
    ((and (mac-os-p) (< emacs-major-version 24)) (ns-toggle-fullscreen))
    ((and (mac-os-p) (require-or-install 'maxframe))
     (if maxframe-fullscreen-p (restore-frame) (maximize-frame))
     (setq maxframe-fullscreen-p (not maxframe-fullscreen-p)))
    (t
     (if (frame-parameter nil 'fullscreen)
         (set-frame-parameter nil 'fullscreen nil)
         (set-frame-parameter nil 'fullscreen 'fullboth)))))

(defun define-keys (&optional mode)
  (loop for (key . fn) in `(("\C-h" . backward-char)
                            ("\C-j" . next-line)
                            ("\C-k" . previous-line)
                            ("\C-l" . forward-char)
                            ("\C-n" . newline-and-indent)
                            (,(kbd "C-'") . recenter)
                            ("\C-f" . anything-next-page)
                            ("\C-b" . anything-previous-page)
                            ("\M-s" . query-replace-regexp)
                            (,(kbd "C--") . font-small)
                            (,(kbd "C-+") . font-big)
                            (,(kbd "C-c m") . toggle-fullscreen))
        if mode do (define-key mode key fn)
        else do (global-set-key key fn)))

(define-keys)

(define-prefix-command 'windmove-map)
(global-set-key (kbd "C-q") 'windmove-map)
(define-key windmove-map "h" 'windmove-left)
(define-key windmove-map "j" 'windmove-down)
(define-key windmove-map "k" 'windmove-up)
(define-key windmove-map "l" 'windmove-right)
(define-key windmove-map "0" 'delete-window)
(define-key windmove-map "1" 'delete-other-windows)
(define-key windmove-map "2" 'split-window-vertically)
(define-key windmove-map "3" 'split-window-horizontally)

(defun split-window-conditional ()
  (interactive)
  (if (> (* (window-height) 2) (window-width))
      (split-window-vertically)
    (split-window-horizontally)))
(define-key windmove-map "s" 'split-window-conditional)

(define-key minibuffer-local-completion-map (kbd "C-w") 'backward-kill-word)
