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


;; Export
(provide 'my-functions)

;;; my-functions.el ends here