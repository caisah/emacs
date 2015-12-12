;;; org-settings.el --- ORG settings
;;; Commentary:
;; Settings for org-mode

;;; Code:

;; enable toc
(setq-default org-src-fontify-natively t)

(eval-after-load "org-toc-autoloads"
  '(progn
     (if (require 'org-toc nil t)
         (add-hook 'org-mode-hook 'org-toc-enable)
       (warn "org-toc not found"))))

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'turn-off-smartparens-mode)

(provide 'org-settings)
;;; org-settings.el ends here
