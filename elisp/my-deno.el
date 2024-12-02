;; -*- lexical-binding: t -*-
;;; deno.el --- Deno related functions


;;; Commentary:
;; A bundle of deno related functions

;;; Code:

(defcustom my-deno-repl-name "*Deno REPL*"
  "Name of the buffer used for deno repl.")

(defun my-deno--create-repl (buffer)
  "Creates a comint BUFFER for deno."
  (apply 'make-comint-in-buffer my-deno-repl-name buffer "deno" nil '("repl")))

(defun my-deno--get-process ()
  "Get deno repl process."
    (get-buffer-process (get-buffer my-deno-repl-name)))

(defun my-deno--display-repl-buffer ()
  "Display deno repl and start comint."
  (let ((buffer (get-buffer-create my-deno-repl-name)))
    (my-deno--create-repl buffer)
    (display-buffer buffer)
    (with-current-buffer buffer
      (setq-local comint-process-echoes t)
      (comint-mode))))


(defun my-deno-run-repl ()
  "Run Deno REPL in a comint buffer."
  (interactive)
  (my-deno--display-repl-buffer))


(defun my-deno--close-repl-buffer ()
  "Set up the current my-deno-repl buffer to close when its process exits."
  (let ((proc (get-buffer-process (current-buffer))))
    (when (and proc (string-equal (buffer-name) my-deno-repl-name))
      (set-process-sentinel
       proc
       (lambda (process event)
         (when (string-match-p "finished\\|exited" event)
           (kill-buffer my-deno-repl-name)))))))


(defun my-deno-send-buffer-to-repl ()
  "Send the contents of a buffer to deno repl."
  (interactive)
  (let ((process (my-deno--get-process))
        (buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
    (if process
        (process-send-string process buffer-contents)
      (progn
        (my-deno--display-repl-buffer)
        (process-send-string process buffer-contents)))))

(defun my-deno-send-region-to-repl ()
  "Send a region to deno repl."
  (interactive)
  (let ((process (my-deno--get-process)))
    (if process
        (let ((region (buffer-substring-no-properties (region-beginning) (region-end))))
          (process-send-string process region)
          (process-send-string process "\n")
          (message "Successfully sent to deno REPL"))
      (message "No active deno REPL"))))

(defun my-deno-reset-repl ()
  "Restart the deno repl."
  (interactive)
  (let* ((buffer my-deno-repl-name)
         (process (get-buffer-process buffer)))
    (if (and buffer process)
        (progn
          (delete-process process)
          (with-current-buffer buffer
            (erase-buffer)
            (my-deno--create-repl buffer)))
      (message "No active deno process"))))


(add-hook 'comint-mode-hook #'my-deno--close-repl-buffer)

(provide 'my-deno)
