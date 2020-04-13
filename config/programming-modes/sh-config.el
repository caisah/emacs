;;; sh-config.el --- Shell config

;;; Commentary:
;; My Shell script config file


;;; Code:

(with-eval-after-load 'sh-script
  (progn
    (message "My init :: sh-mode loaded")))

(add-hook 'sh-mode-hook 'company-mode)
(add-hook 'sh-mode-hook 'flycheck-mode)

(provide 'sh-config)
;;; sh-config.el ends here
