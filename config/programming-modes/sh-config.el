;;; sh-config.el --- Bash config

;;; Commentary:
;; My Shell script config file


;;; Code:
(add-hook 'sh-mode-hook 'flycheck-mode)
(add-hook 'sh-mode-hook 'linum-mode)

(with-eval-after-load 'sh-script
  (progn
    (message "sh-mode loaded")))


(provide 'sh-config)
;;; bash-config.el ends here
