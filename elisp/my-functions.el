;; -*- lexical-binding: t -*-
;;; my- functions.el --- My custom functions


;;; Commentary:
;; A bundle of unrelated functions

;;; Code:
(defun dired-go-up-dir ()
  "Navigates to the parent dir."
  (interactive)
  (find-alternate-file ".."))


(defun time-passed-since (start)
  "Return the number of seconds from START until the current time."
  (let* ((end (current-time))
         (s-lo (cadr start))
         (s-us (nth 2 start))
         (e-lo (cadr end))
         (e-us (nth 2 end)))
    (+ (- e-lo s-lo)
       (/ (- e-us  s-us) 1e6))))

(defun my-init-message (name)
  "Writes  nice load message in Message buffer for the NAME package."
  (message "My init :: package \"%s\" loaded" name))

(defun my-wrap-with-round-paren (&optional arg)
  "Wrap ARG with in round parentheis ()."
  (interactive "p")
  (sp-select-next-thing-exchange arg)
  (execute-kbd-macro (kbd "(")))

(defun my-current-buffer-too-big-p ()
  "Check if a buffer is really big."
  (or (> (buffer-size) (* 5000 80))
      (> (line-number-at-pos (point-max)) 5000)))

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
  (let ((buff (buffer-file-name)))
    (if buff (progn
               (kill-new buff)
               (message "Copied: %s" buffer-file-name))
      (message "No file to copy in: %s" major-mode))))

(defun my-read-file (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun my-display-code-line-counts (ov)
                  (when (eq 'code (overlay-get ov 'hs))
                    (overlay-put ov 'display
                                 (propertize
                                  (format " ... %d lines ... "
                                          (count-lines (overlay-start ov)
                                                       (overlay-end ov)))
                                  'face 'hs-face))))

(defun index-template (name)
  (format  (my-read-file "~/.emacs.d/config/elisp/templates/index.ts") name))

(defun styles-template (name)
  (format  (my-read-file "~/.emacs.d/config/elisp/templates/styles.ts") name))


(defun main-template (name)
  (format (my-read-file "~/.emacs.d/config/elisp/templates/main.ts") name))

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

;; opening shells
(defun my-shell-here ()
  "Open shell buffer in this window."
  (interactive)
  (let* ((dir (or dired-directory default-directory))
         (name (car (last (split-string dir "/") 2))))

    (eshell "")
    (rename-buffer (concat "*" name " shell*"))))

(defun my-shell-other ()
  "Open shell buffer in the other window."
  (interactive)
  (progn
    (other-window 1)
    (my-shell-here)))

(defun my-shell-split-up ()
  "Split other window up and open shell buffer."
  (interactive)
  (progn
    (other-window 1)
    (split-window-below)
    (my-shell-here)))

(defun my-shell-split-below ()
  "Split other window down and open shell buffer."
  (interactive)
  (progn
    (other-window 1)
    (split-window-below)
    (other-window 1))
    (my-shell-here))

(defun my-consult-line ()
  "Call `consult-line` with the selected string."
  (interactive)
  (if (region-active-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (substring  (buffer-substring start end)))
        (deactivate-mark)
        (consult-line substring))
    (consult-line)))

(defun my-erc-start ()
  "Start ERC, Join Freenode & Snoonet."
  (interactive)

  (erc-tls :server "irc.snoonet.org" :port 6697 :nick "caisah" :full-name "Caisah")
  (erc-tls :server "irc.freenode.net" :port 6697 :nick "caisah" :full-name "Caisah"))


(defun my-js-repl-open-other-window ()
  "Open js REPL in a new window."
  (interactive)
  (js-comint-repl js-comint-module-paths)
  (other-window -1))

(defun my-js-repl-reset ()
  "Reset and clears js REPL."
  (interactive)
  (let ((my-last-window (selected-window)))
    (js-comint-reset-repl)
    (js-comint-clear)
    (select-windo my-last-window)))

(defun my-open-file-in-browser (file)
  "Open FILE in the default web browser."
  (interactive)
  (if (file-exists-p file)
      (browse-url-firefox (concat "file://" file) "-a")
    (message "File does not exist: %s" file)))

(defun my-open-buffer-fine-in-browser ()
  "Open the file associated to the cu in default browser."
  (interactive)
  (my-open-file-in-browser (buffer-file-name)))

(defun my-install-treesit-languages ()
  "Install/build all treesit languages."
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

(provide 'my-functions)
;;; my-functions.el ends here
