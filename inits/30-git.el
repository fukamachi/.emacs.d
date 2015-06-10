(defun git-blame-current-line ()
  (interactive)
  (let ((blame-result
         (shell-command-to-string
          (format "git blame -p -L %d,+1 %s" (line-number-at-pos) (buffer-file-name))))
        (result (make-hash-table :test 'equal)))
    (loop with (commit . lines) = (split-string blame-result "\n")
          for line in lines
          for (key . values) = (split-string (or line "") " ")
          do (setf (gethash key result) (mapconcat 'identity values " "))
          finally
       (setf (gethash "commit" result) commit))
    (funcall (if (fboundp 'popup-tip)
                 'popup-tip
                 'message)
     (format "[%s] %s: %s (%s)"
             (substring (gethash "commit" result) 0 6)
             (gethash "author" result)
             (gethash "summary" result)
             (format-time-string "%Y/%m/%d %H:%M"
                                 (seconds-to-time
                                  (string-to-number (gethash "author-time" result))))))))
(add-hook 'server-vist-hook
          (lambda ()
            (when (string-match "COMMIT_EDITMSG" buffer-file-name)
              (set-buffer-file-coding-system 'utf-8))))

(el-get 'sync 'git-status)
(require 'git-status)
