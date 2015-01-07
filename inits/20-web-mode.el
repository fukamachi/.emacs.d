(require-or-install 'web-mode)

(appendf auto-mode-alist
         '(("\\.\\(html?\\|emb\\|tmpl\\|tt\\)$" . web-mode)
           ("\\.jsx$" . web-mode)))
