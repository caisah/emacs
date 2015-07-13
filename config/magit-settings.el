;;; magit-settings.el --- Magit settings

;;; Commentary:
;; https://github.com/magit/magit

;;; Code:
(require 'magit)

(setq magit-last-seen-setup-instructions "1.4.0")
(set-default 'magit-stage-all-confirm nil)
(set-default 'magit-unstage-all-confirm nil)
(setq magit-auto-revert-mode nil)

(provide 'magit-settings)
;;; magit-settings.el ends here
