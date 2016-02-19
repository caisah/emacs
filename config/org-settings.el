;;; org-settings.el --- ORG settings
;;; Commentary:
;; Settings for org-mode

;;; Code:

;; enable toc
(setq-default org-src-fontify-natively t)

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'turn-off-smartparens-mode)

(provide 'org-settings)
;;; org-settings.el ends here
