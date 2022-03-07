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

(defun my-copy-buffer-file-name ()
  "Copy buffer file name."
  (interactive)
  (kill-new (buffer-file-name))
  (message "Copied: %s" buffer-file-name))

(defun index-template (name)
  (format "export { default } from './%s';" name))

(defun styles-template (name)
  (format "import { createUsestyles } from 'react-jss';\n\nexport default createUseStyles({\nroot:{\n\n}\n}, { name: '%s'});" name))

(defun main-template (name)
  (format "export default function %s({}){\n\n};" name))

(defun my-new-ts-component (name)
  "New NAME ts component."
  (interactive "sComponent: ")
  (progn
    (let ((types-file (concat name "/types.ts"))
          (styles-file (concat name "/styles.ts"))
          (main-file (concat name "/" name ".tsx"))
          (index-file (concat name "/index.ts")))
      (make-empty-file types-file)
      (make-empty-file styles-file)
      (make-empty-file main-file)
      (make-empty-file index-file)

      (append-to-file (index-template name) nil index-file)
      (append-to-file (styles-template name) nil styles-file)
      (append-to-file (main-template name) nil main-file)
      (message (concat "Created component " name)))))

;; Export
(provide 'my-functions)

;;; my-functions.el ends here
