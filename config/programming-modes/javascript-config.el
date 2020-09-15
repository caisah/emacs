;;; javascript-config.el --- JavaScript config

;;; Commentary:
;; My JavaScript config file

;;; Notes to self:
;;; add hippie expand `hippie-expand-try-functions-list

;;; Code:
(defvar my-standard-exec nil
  "Local variable holding the executable for javascript standard.")

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-mode))
(add-to-list 'interpreter-mode-alist '("node" . js-mode))

(defun my-poor-standard-format ()
  "Formats buffer with prettier standard.
This is very rudimentary implementation."
  (let ((buffer (buffer-file-name)))
    (if buffer
        (progn
          (call-process my-standard-exec nil nil nil "--fix" buffer)
          (revert-buffer nil t t))
      (message "Formatting does not work yet. Save file first!"))))

(defun my-find-js-executable-in-nm (nm-exec-name)
  "Search for the NM-EXEC-NAME executable in node_modules.

It first tries to find a `node_modules' dir starting with current buffer
and then tests for the existance of the exec in `.bin' directory.

Returns the full path of the executable."

  (let* ((start (or (buffer-file-name) default-directory))
         (dir
          (locate-dominating-file
           start
           (lambda (search-dir)
             (let ((nm-dir (concat search-dir "node_modules")))
               (and
                (file-directory-p nm-dir)
                (file-executable-p (concat nm-dir "/.bin/" nm-exec-name))))))))
    (when dir
      (concat dir "node_modules/.bin/" nm-exec-name))))

(defun my-try-use-standard ()
  "Try to use  `standard' for both `flycheck' and reformat.
If `standard' is found in local node_modules is used as
 `flycheck-javascript-standard-executable'."

  (let ((standard-exec (my-find-js-executable-in-nm "standard")))
    (when standard-exec
      (progn
        (print standard-exec)
        (setq-local flycheck-javascript-standard-executable standard-exec)
        (add-hook 'after-save-hook 'my-poor-standard-format)
        (setq-local my-standard-exec standard-exec)))))

(defun my-try-use-local-prettier ()
  "Set `prettier-js-command' to local node_modules executable."
  (let ((prettier-exec (my-find-js-executable-in-nm "prettier")))
    (when (and prettier-exec (file-executable-p prettier-exec))
      (progn
        (setq-local prettier-js-command prettier-exec)
        (prettier-js-mode 1)))))

(defun my-try-use-eslint ()
  "Set `flycheck-javascript-eslint-executable' to local node_modules executable."
  (let ((eslint-exec (my-find-js-executable-in-nm "eslint")))
    (when (and eslint-exec (file-executable-p eslint-exec))
      (setq-local flycheck-javascript-eslint-executable eslint-exec))))

(defun my-try-use-prettier-eslint ()
  "Try to use local or global `prettier-eslint'."
  (let ((prettier-eslint
         (or (my-find-js-executable-in-nm "prettier-eslint")
             (executable-find "prettier-eslint"))))
    (when prettier-eslint
      (progn
        (setq-local prettier-js-command prettier-eslint)
        (prettier-js-mode 1)))))

(defun my-setup-js-checker-formater ()
  "Setup error checker and formatter for javascript.
The order is: `standard', `eslint' `prettier' `prettier-eslint'."
  (or (my-try-use-standard)
      (my-try-use-eslint)
      (or (my-try-use-local-prettier)
        (my-try-use-prettier-eslint))))

(defun my-company-transformer (candidates)
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun my-js-repl-open-other-window ()
  "Open js REPL in a new window."
  (interactive)
  (js-comint-repl js-comint-module-paths)
  (other-window -1))

(defun my-js-repl-reset ()
  "Reset and clears js REPL."
  (interactive)
  (js-comint-reset-repl)
  (js-comint-clear)
  (other-window -1))

(defun my-js-specific ()
  "Personal hook for \"js-mode\" major mode."
  (if (eq major-mode 'js-mode)
    (progn
      ;; Enable autocomplete
      (company-mode 1)
      ;; Enable yasnippet
      (yas-minor-mode 1)
      ;; Enable code navigation
      (lsp t)
      ;; Don't consider camelcased full words
      (subword-mode 1)
      ;; Don't consider underscored full words
      (superword-mode 1)
      ;; Use editor config
      (editorconfig-mode 1)

      ;; Try to use eslint & prettier
      (my-setup-js-checker-formater)

      (setq-local company-backends
                  '(company-lsp
                    company-dabbrev-code
                    company-dabbrev
                    company-files
                    company-keywords))

      ;; Change mode name to JS
      (setq mode-name "JS")

      ;; for lsp-javascript
      (make-local-variable 'company-transformers)
      (push 'my-company-transformer company-transformers)

      (local-set-key (kbd "C-c C-f") 'hs-toggle-hiding)
      (local-set-key  (kbd "M-.") 'xref-find-definitions)
      (local-set-key  (kbd "C-,") 'lsp-find-references)

      (local-set-key (kbd "C-c C-r") 'my-js-repl-open-other-window)
      (local-set-key (kbd "C-c C-e") 'js-comint-send-last-sexp)
      (local-set-key (kbd "C-c C-b") 'js-comint-send-buffer)
      (local-set-key (kbd "C-c C-c") 'my-js-repl-reset)
      )))

(with-eval-after-load 'js
  (progn
    (message "My init :: js-mode loaded")

    (setq-default js-indent-level 2)))

;; Set config for prettier code formatter
(with-eval-after-load 'prettier-js
  (progn
    (message "My init :: prettier js loaded")

    (setq-default prettier-js-args '("--trailing-comma" "all"
                                     "--bracket-spacing" "true"
                                     "--single-quote" "true"))))

(add-hook 'js-mode-hook 'my-js-specific)
(add-hook 'js-mode-hook 'whitespace-mode)
(add-hook 'js-mode-hook 'smartparens-strict-mode)
(add-hook 'js-mode-hook 'hs-minor-mode)
(add-hook 'js-mode-hook 'flycheck-mode)

;; Export
(provide 'javascript-config)
;;; javascript-config.el ends here
