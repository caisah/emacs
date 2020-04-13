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


(defun my-show-startup-time ()
  "Message the time since .init file loaded."
  (let ((time-passed (time-passed-since *start-time*)))
    (run-with-timer 1 nil (lambda (time) (message "My .init loaded in %.3f seconds. Happy hacking!" time)) time-passed)))

(defun my-kill-other-buffer ()
  "Kill buffer in the other window."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window 1))

(defun my-kill-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc
   'kill-buffer
   (buffer-list)))

(defun my-delete-to-previous-line ()
  "Delete to previous end of line."
  (interactive)
  (delete-horizontal-space)
  (backward-delete-char 1)
  (delete-horizontal-space)
  (insert-char 32))


;; Export
(provide 'my-functions)

;;; my-functions.el ends here
