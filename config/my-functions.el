(defun time-passed-since (start)
  "Returns the number of seconds from START until the current time"
  (let* ((end (current-time))
         (s-lo (cadr start))
         (s-us (nth 2 start))
         (e-lo (cadr end))
         (e-us (nth 2 end))) 
    (+ (- e-lo s-lo)
       (/ (- e-us  s-us) 1e6))))

(defun display-startup-echo-area-message ()
  (let ((time-passed (time-passed-since *start-time*)))
    (message "My .init loaded in %.3f seconds. Happy hacking!" time-passed)))

(defun kill-other-buffer ()
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window 1))

(defun delete-to-previous-line ()
  (interactive)
  (delete-horizontal-space)
  (backward-delete-char 1)
  (delete-horizontal-space)
  (insert-char 32))

(defun yank-next (&optional arg)
 (interactive)
  (end-of-line)
  (newline-and-indent)
  (yank arg))

(provide 'my-functions)
