;;; typescript-config.el --- Typescript config

;;; Commentary:
;; My TypeScript config file

;;; Code:
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(defun my-tshook ()
  "Personal hook for js2-mode."
  (progn
    (setq-local company-backends
                '(company-dabbrev-code
                  company-dabbrev
                  company-lsp
                  company-files
                  company-keywords))
    ;; Change mode name to JS2
    (setq mode-name "TypeScript")
    ;; Don't consider camelcased full words
    (subword-mode 1)
    ;; Don't consider underscored full words
    (superword-mode 1)
    ;; Override js2-mode toggle
    (local-set-key (kbd "C-c C-f") 'hs-toggle-hiding)
    (local-set-key  (kbd "M-.") 'xref-find-definitions)
    ))

;; Do setting recommended configuration
(add-hook 'typescript-mode-hook 'pretty-symbols-mode)
(add-hook 'typescript-mode-hook 'linum-mode)
(add-hook 'typescript-mode-hook 'abbrev-mode)
(add-hook 'typescript-mode-hook 'smartparens-strict-mode)
(add-hook 'typescript-mode-hook 'hs-minor-mode)
(add-hook 'typescript-mode-hook 'whitespace-mode)
(add-hook 'typescript-mode-hook 'company-mode)
(add-hook 'typescript-mode-hook 'lsp-javascript-typescript-enable)
(add-hook 'typescript-mode-hook 'flycheck-mode)
(add-hook 'typescript-mode-hook 'my-tshook)

(with-eval-after-load 'typescript-mode
  (progn
    (message "typescript-mode loaded")

    (require 'lsp-javascript-typescript)))

;; Export
(provide 'typescript-config)
;;; typescript-config.el ends here
