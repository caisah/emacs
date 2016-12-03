;;; shell-settings.el --- Shell

;;: Commentary:
;; Settings for M-x shell

;;; Code:
(defun my-shell ()
  "Open shell when in dired mode.
Does the job when using `dired-maybe-insert-subdir`"
  (interactive)
  (if (string-equal major-mode "dired-mode")
    (let* ((dir (dired-current-directory))
           (default-directory dir)
           (dir-name (car (last (split-string dir "/") 2)))
           (buffer-name (format "shell /%s*" dir-name)))
      (shell buffer-name))
    (shell)))

(defun my-shell-execute-last-command ()
  "Run last command in shell buffer."
  (interactive)
  (when (get-buffer "*shell*")
    (with-current-buffer "*shell*"
      (end-of-buffer)
      (comint-send-input (comint-previous-input 1))
      (message "Last command executed"))))


;; set key for opening a new shell
(global-set-key (kbd "C-c s") 'my-shell)

;; show full width lines in shell mode
(add-hook 'shell-mode-hook '(lambda ()
                              (visual-line-mode nil)
                              (setq truncate-lines t)))

;; strip ugly characters whe working in shell
(add-to-list
 'comint-preoutput-filter-functions
 (lambda (output)
   (replace-regexp-in-string "\\[\\??[0-9]+[GhK]" "" output)))


(provide 'shell-settings)
;;; shell-settings ends here
