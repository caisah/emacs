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

(defun my-open-file-in-browser (file)
  "Open FILE in the default web browser."
  (interactive)
  (if (file-exists-p file)
      (browse-url-firefox (concat "file://" file) "-a")
    (message "File does not exist: %s" file)))

(defun my-open-buffer-file-in-browser ()
  "Open the file associated to the buffer in default browser."
  (interactive)
  (my-open-file-in-browser (buffer-file-name)))

(defun my-deno-project-p ()
  "Determine if inside a deno project."
  (when (locate-dominating-file "." "deno.json") t))

(defun my-ts-server-program (&rest _)
  "Decide which server to use based on project characteristics."
  (cond ((my-deno-project-p) '("deno" "lsp" :initializationOptions '(:enable t :lint t)))
        (t `("typescript-language-server" "--stdio"))))

(defun my-local-oxlint-executable ()
  "Return local oxlint path if available, otherwise nil."
  (when-let* ((root (locate-dominating-file default-directory "node_modules"))
              (oxlint (expand-file-name "node_modules/.bin/oxlint" root)))
    (and (file-executable-p oxlint) oxlint)))

(defun my-stylelint-executable ()
  "Return project stylelint if present, otherwise global stylelint."
  (or (when-let* ((root (locate-dominating-file default-directory "node_modules"))
                  (local-stylelint (expand-file-name "node_modules/.bin/stylelint" root)))
        (and (file-executable-p local-stylelint) local-stylelint))
      (executable-find "stylelint")))

(defun my-use-lint-from-node-modules ()
  "Configure Flycheck to use local oxlint or eslint from node_modules."
  (interactive)
  (when-let* ((root (locate-dominating-file default-directory "node_modules"))
              (bin-dir (expand-file-name "node_modules/.bin/" root)))

    (let ((oxlint (expand-file-name "oxlint" bin-dir))
          (eslint (expand-file-name "eslint" bin-dir)))

      (cond
       ;; Priority 1: oxlint (it's faster)
       ((file-executable-p oxlint)
        (setq-local flycheck-javascript-oxlint-executable oxlint)
        (flycheck-select-checker 'javascript-oxlint))

       ;; Priority 2: eslint
       ((file-executable-p eslint)
        (setq-local flycheck-javascript-eslint-executable eslint)
        (flycheck-select-checker 'javascript-eslint))))))

(defun my-quit-eldoc-buffer ()
  "Quits an eldoc window."
  (interactive)
  (let ((eldoc-buffer (get-buffer "*eldoc*")))
    (when eldoc-buffer
      (quit-window (select-window (get-buffer-window eldoc-buffer))))))


(defun my-prog-modes ()
  (abbrev-mode 1)
  (company-mode 1)
  (editorconfig-mode 1)
  (eglot-ensure)
  (hs-minor-mode 1)
  (prettify-symbols-mode 1)
  (subword-mode 1)
  (superword-mode 1)
  (whitespace-mode 1)
  (yas-minor-mode 1)
  (yas-reload-all)
  (rainbow-delimiters-mode 1)
  (flycheck-mode 1))

(defun my-css-mode-setup ()
  (setq-local eglot-stay-out-of '(flymake))
  (my-prog-modes)
  (when-let ((stylelint (my-stylelint-executable)))
    (setq-local flycheck-css-stylelint-executable stylelint)
    (flycheck-select-checker 'css-stylelint)))

(defun my-open-special-markdown-link-at-point ()
  "Open the Markdown link at point in the `eldoc` buffer."
  (interactive)
  (let ((url (get-text-property (point) 'help-echo)))
    (if url
        (browse-url url)
      (message "No URL found at point"))))

(defun my-typescript-general-hook ()
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  (when (my-deno-project-p)
    (setq-local apheleia-formatter 'denofmt)
    (apheleia-mode 1))

  (when (my-local-oxlint-executable)
    (setq-local eglot-stay-out-of '(flymake)))
  (my-prog-modes))

(defmacro def-pairs (pairs)
  "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
  `(progn
     ,@(cl-loop for (key . val) in pairs
                collect
                `(defun ,(read (concat
                                "wrap-with-"
                                (prin1-to-string key)
                                "s"))
                     (&optional arg)
                   (interactive "p")
                   (sp-wrap-with-pair ,val)))))

(def-pairs ((paren . "(")
            (bracket . "[")
            (brace . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote . "`")))

(provide 'my-functions)
;;; my-functions.el ends here
