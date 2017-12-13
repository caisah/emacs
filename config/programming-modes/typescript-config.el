;;; typescript-config.el --- Typescript config

;;; Commentary:
;; My TypeScript config file

;;; Code:
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;; Do setting recommended configuration
(add-hook 'typescript-mode-hook 'pretty-symbols-mode)
(add-hook 'typescript-mode-hook 'linum-mode)
(add-hook 'typescript-mode-hook 'abbrev-mode)
(add-hook 'typescript-mode-hook 'smartparens-strict-mode)
(add-hook 'typescript-mode-hook 'hs-minor-mode)
(add-hook 'typescript-mode-hook 'flycheck-mode)
(add-hook 'typescript-mode-hook 'whitespace-mode)
(add-hook 'typescript-mode-hook 'company-mode)

;; Export
(provide 'typescript-config)
;;; typescript-config.el ends here
