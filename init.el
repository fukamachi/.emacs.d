(defvar *emacs-config-directory* (file-name-directory load-file-name))

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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

;; el-get
(defvar *el-get-directory*
  (expand-file-name "el-get/el-get" *emacs-config-directory*))

(add-to-list 'load-path *el-get-directory*)

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(require 'el-get)

(setq el-get-recipe-path
      (list (expand-file-name "recipes" *el-get-directory*)
            (expand-file-name "el-get/user/recipes" *emacs-config-directory*)))

(setq el-get-user-package-directory
      (expand-file-name "el-get/user/init-files" *emacs-config-directory*))

(el-get 'sync)

;; init-loader

(package-initialize)
(require-or-install 'init-loader)

(setq init-loader-show-log-after-init nil)

(init-loader-load
 (expand-file-name "inits/" *emacs-config-directory*))
