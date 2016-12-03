;;; go-config.el --- Go lang config

;;; Commentary:
;; My go lang config file

(defun go-gonfig-set-local-variables ()
  (setq tab-width 2))

(add-hook 'go-mode-hook 'linum-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'go-gonfig-set-local-variables)

(with-eval-after-load 'go-mode
  (progn
    (message "go-mode loaded")))

;; Export
(provide 'go-config)
;;; javascript-config.el ends here
