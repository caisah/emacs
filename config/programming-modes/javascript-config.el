;;; javascript-config.el --- JavaScript config

;;; Commentary:
;; My JavaScript config file

;;; Notes to self:
;;; add hippie expand `hippie-expand-try-functions-list

;;; Code:
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))


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

(defun my-company-transformer (candidates)
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun my-jshook ()
  "Personal hook for js2-mode."
  (progn
    (setq-local company-backends
                '(company-dabbrev-code
                  company-dabbrev
                  company-lsp
                  company-files
                  company-keywords))
    ;; Enable yasnippet
    (yas-minor-mode)
    (yas-reload-all)
    ;; Change mode name to JS2
    (setq mode-name "JS2")
    (my-use-eslint-from-node-modules)
    ;; Don't consider camelcased full words
    (subword-mode 1)
    ;; Don't consider underscored full words
    (superword-mode 1)
    ;; Use editor config
    (editorconfig-mode 1)
    ;; Override js2-mode toggle

    ;; for lsp-javascript
    (make-local-variable 'company-transformers)
    (push 'my-company-transformer company-transformers)

    (local-set-key (kbd "C-c C-f") 'hs-toggle-hiding)
    (local-set-key  (kbd "M-.") 'xref-find-definitions)
    ))

(add-hook 'js2-mode-hook 'my-jshook)
(add-hook 'js2-mode-hook 'whitespace-mode)
(add-hook 'js2-mode-hook 'company-mode)
(add-hook 'js2-mode-hook 'smartparens-strict-mode)
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook 'hs-minor-mode)
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'linum-mode)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'js2-mode-hook 'lsp)

(with-eval-after-load 'js2-mode
  (progn
    (message "js2-mode loaded")

    (setq-default
     js2-mode-show-parse-errors nil
     js2-mode-show-strict-warnings nil
     js2-strict-var-redeclaration-warning nil
     js2-strict-missing-semi-warning nil
     js2-mode-show-strict-warnings nil
     js2-include-browser-externs t
     js2-include-node-externs t
     js2-basic-offset 2)))

;; Set config for prettier code formatter
(with-eval-after-load 'prettier-js
  (progn
    (message "prettier js loaded")

    (setq-default prettier-js-args '("--trailing-comma" "all"
                                     "--bracket-spacing" "true"
                                     "--single-quote" "true"))))

;; Use google-chrome for indium
(setq-default indium-chrome-executable "chromium-browser")

;; Export
(provide 'javascript-config)
;;; javascript-config.el ends here
