(require-or-install 'evil)
(evil-mode 1)

(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'previous-line)
(define-key evil-insert-state-map (kbd "C-j") 'next-line)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(define-key evil-insert-state-map (kbd "<return>") 'evil-ret-and-indent)

(define-key evil-normal-state-map "D"
  (lambda ()
    (interactive)
    (if (and (boundp 'paredit-mode) paredit-mode)
        (kill-sexp)
      (kill-line))))

(define-key evil-normal-state-map "C"
  (lambda ()
    (interactive)
    (if (and (boundp 'paredit-mode) paredit-mode)
        (kill-sexp)
      (kill-line))
    (evil-insert 1)))

(evil-define-key 'normal paredit-mode ")" 'paredit-forward-up)
(evil-define-key 'normal paredit-mode "(" 'paredit-backward-up)
(evil-define-key 'normal paredit-mode (kbd "C-0") 'paredit-backward-down)
(evil-define-key 'normal paredit-mode (kbd "C-9") 'paredit-forward-down)

(eval-after-load "slime"
  '(progn
     (define-key evil-normal-state-map (kbd "M-.") 'slime-edit-definition)
     (define-key evil-normal-state-map (kbd "M-,") 'slime-pop-find-definition-stack)))
