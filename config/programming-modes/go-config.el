;;; go-config.el --- Go lang config

;;; Commentary:
;; My go lang config file

;;; Code:
(defun my-go-hook ()
  "My personal golang hook."
  (setq tab-width 2))

(add-hook 'go-mode-hook 'linum-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'my-go-hook)

(with-eval-after-load 'go-mode
  (progn
    (message "go-mode loaded")))

;; Export
(provide 'go-config)
;;; go-config.el ends here
