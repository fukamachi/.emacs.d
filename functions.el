(defun my-set-cursor (cursor-shape)
  "Set the terminal cursor shape."
  (unless (display-graphic-p)
    (send-string-to-terminal
     (cond
      ((eq cursor-shape 'box) "\e[2 q")
      ((eq cursor-shape 'bar) "\e[6 q")
      ((eq cursor-shape 'hbar) "\e[4 q")))))

(defun my-set-cursor-type-for-evil-state ()
  "Change cursor type based on Evil state."
  (cond
   ((evil-normal-state-p)  (my-set-cursor 'box))
   ((evil-insert-state-p)  (my-set-cursor 'bar))
   ((evil-replace-state-p) (my-set-cursor 'hbar))
   (t                      (my-set-cursor 'box))))

(defun my-unwrap-sexp ()
  (interactive)
  (save-excursion
    (sp-backward-up-sexp)
    (sp-unwrap-sexp)))

(defun my-close-paren ()
  (interactive)
  (sp-up-sexp)
  (backward-char))

(defun my-raise-sexp ()
  "Replace the parent S-expression with the current S-expression."
  (interactive)
  (save-excursion
    (let ((current-sexp (thing-at-point 'sexp t)))
      (backward-up-list)
      (kill-sexp)
      (insert current-sexp))))

(defun set-lisp-keybindings (map)
  (evil-define-key 'normal map
    "(" 'sp-backward-up-sexp
    ")" 'my-close-paren
    (kbd "SPC w") 'sp-wrap-round
    (kbd "SPC i") 'sp-wrap-round
    (kbd "SPC @") 'my-unwrap-sexp
    (kbd "M-l") 'sp-forward-slurp-sexp
    (kbd "M-h") 'sp-forward-barf-sexp
    (kbd "SPC o") 'my-raise-sexp))
