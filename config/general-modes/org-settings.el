;;; org-settings.el --- ORG settings
;;; Commentary:
;; Settings for org-mode

;;; Code:

;; Use org package
(straight-use-package 'org)
;; enable toc
(with-eval-after-load 'org
  (progn
    (message "org-mode loaded")

    (setq org-src-fontify-natively t)))

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'turn-off-smartparens-mode)

(provide 'org-settings)
;;; org-settings.el ends here
