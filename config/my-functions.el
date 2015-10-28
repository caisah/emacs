;;; my-functions.el --- My custom functions

;;; Commentary:
;; A bundle of unrelated functions

;;; Code:
(defun time-passed-since (start)
  "Return the number of seconds from START until the current time."
  (let* ((end (current-time))
         (s-lo (cadr start))
         (s-us (nth 2 start))
         (e-lo (cadr end))
         (e-us (nth 2 end)))
    (+ (- e-lo s-lo)
       (/ (- e-us  s-us) 1e6))))

(defun display-startup-echo-area-message ()
  "Message the time since .init file loaded."
  (let ((time-passed (time-passed-since *start-time*)))
    (message "My .init loaded in %.3f seconds. Happy hacking!" time-passed)))

(defun kill-other-buffer ()
  "Kill buffer in the other window."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window 1))

(defun delete-to-previous-line ()
  "Delete to previous end of line."
  (interactive)
  (delete-horizontal-space)
  (backward-delete-char 1)
  (delete-horizontal-space)
  (insert-char 32))

(defun yank-next (&optional arg)
  "Yank ARG on the next line."
 (interactive)
  (end-of-line)
  (newline-and-indent)
  (yank arg))

(defun shell-execute-last-command ()
  "Run last command in shell buffer."
  (interactive)
  (when (get-buffer "*shell*")
    (with-current-buffer "*shell*"
      (end-of-buffer)
      (comint-send-input (comint-previous-input 1))
      (message "Last command executed"))))

;; Saving:
(defun run-before-buffer-save ()
  "Run this before saving a buffer."
  (delete-trailing-whitespace))

(defun my-save-buffer ()
  "Delete trailing whitespaces and then save buffer."
  (interactive)
  (run-before-buffer-save)
  (save-buffer))


;; Export
(provide 'my-functions)

;;; my-functions.el ends here
