;;; typescript-config.el --- Typescript config

;;; Commentary:
;; My TypeScript config file

;;; Code:
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(defun my-tshook ()
  "Personal hook for typescript-mode."

  ;; Change mode name to TypeScript
  (setq mode-name "TypeScript")

  ;; Enable TIDE
  ;; (tide-setup)

  ;; Enable flycheck & eldoc
  (flycheck-mode +1)
  (eldoc-mode +1)

  (lsp)
  ;; Enable yasnippet
  (yas-reload-all)
  (yas-minor-mode-on)

  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; Do setting recommended configuration
(add-hook 'typescript-mode-hook 'my-tshook)
(add-hook 'typescript-mode-hook 'pretty-symbols-mode)
(add-hook 'typescript-mode-hook 'abbrev-mode)
(add-hook 'typescript-mode-hook 'smartparens-strict-mode)
(add-hook 'typescript-mode-hook 'hs-minor-mode)
(add-hook 'typescript-mode-hook 'whitespace-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'company-mode)

(with-eval-after-load 'typescript-mode
  (progn
    (require 'eslint-fix)
    (message "typescript-mode loaded")))

;; Export
(provide 'typescript-config)
;;; typescript-config.el ends here
