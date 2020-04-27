;;; javascript-config.el --- JavaScript config

;;; Commentary:
;; My JavaScript config file

;;; Notes to self:
;;; add hippie expand `hippie-expand-try-functions-list

;;; Code:
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-mode))
(add-to-list 'interpreter-mode-alist '("node" . js-mode))


(defun my-use-eslint-from-node-modules ()
  "Try to use local .eslint file instead of global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))


(defun my-enable-standard-if-possible ()
  "Try to use standard if exists.
Return t on success, nil on failure."
  (let* ((root (locate-dominating-file (or (buffer-file-name) default-directory) "node_modules"))
         (standard (and (concat root "/node_modules/.bin/standard"))))

    (if (file-executable-p standard)
        (progn (setq-local flycheck-javascript-standard-executable standard)
               (flycheck-select-checker 'javascript-standard)
               (setq prettier-js-command "prettier-standard")
               t))))

(defun my-company-transformer (candidates)
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun my-js-specific ()
  "Personal hook for \"js-mode\" major mode."
  (if (eq major-mode 'js-mode)
    (progn
      ;; Enable autocomplete
      (company-mode 1)
      ;; Enable auto formatting
      (prettier-js-mode 1)
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

      (setq-local company-backends
                  '(company-lsp
                    company-dabbrev-code
                    company-dabbrev
                    company-files
                    company-keywords))

      ;; Change mode name to JS
      (setq mode-name "JS")
      (unless (my-enable-standard-if-possible)
        (my-use-eslint-from-node-modules))

      ;; for lsp-javascript
      (make-local-variable 'company-transformers)
      (push 'my-company-transformer company-transformers)

      (local-set-key (kbd "C-c C-f") 'hs-toggle-hiding)
      (local-set-key  (kbd "M-.") 'xref-find-definitions)
      (local-set-key  (kbd "C-,") 'lsp-find-references)
      )))

(with-eval-after-load 'js
  (progn
    (message "My init :: js-mode loaded")

    (setq-default
     ;; Don't use flymake with lsp
     lsp-prefer-flymake :none
     js-indent-level 2
)))

;; Set config for prettier code formatter
(with-eval-after-load 'prettier-js
  (progn
    (message "My init :: prettier js loaded")

    (setq-default prettier-js-args '("--trailing-comma" "all"
                                     "--bracket-spacing" "true"
                                     "--single-quote" "true"))))

;; Use google-chrome for indium
(setq-default indium-chrome-executable "chromium-browser")

(add-hook 'js-mode-hook 'my-js-specific)
(add-hook 'js-mode-hook 'whitespace-mode)
(add-hook 'js-mode-hook 'smartparens-strict-mode)
(add-hook 'js-mode-hook 'hs-minor-mode)
(add-hook 'js-mode-hook 'flycheck-mode)

;; Export
(provide 'javascript-config)
;;; javascript-config.el ends here
