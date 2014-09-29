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
    (message "\n\nMy .init loaded in %.3f seconds\n" time-passed)))


(provide 'snippets)
