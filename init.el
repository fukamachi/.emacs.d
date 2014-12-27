(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defun package-install-with-refresh (package)
  (unless (assq package package-alist)
    (package-refresh-contents))
  (unless (package-installed-p package)
    (package-install package)))

(defun require-or-install (package)
  (or (require package nil t)
      (progn
       (package-install-with-refresh package)
       (require package))))

(require-or-install 'init-loader)

(setq init-loader-show-log-after-init nil)

(init-loader-load
 (expand-file-name "inits/" (file-name-directory load-file-name)))

;; el-get
(defvar *el-get-directory*
  (expand-file-name "el-get/el-get" (file-name-directory load-file-name)))

(unless (file-exists-p *el-get-directory*)
  (let ((buffer (url-retrieve-synchronously
                 "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'load-path *el-get-directory*)
(require 'el-get)
(add-to-list 'el-get-recipe-path
             (expand-file-name "recipes" *el-get-directory*))
(el-get 'sync)
