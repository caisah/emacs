;;; go-config.el --- Golang config

;;; Commentary:
;; My golang config file

;;; Code:
(defun my-go-hook ()
  "Hook for go lang."
  (progn
    (set (make-local-variable 'compile-command)
	 (concat "go run -v" (buffer-file-name)))

    (lsp)

    (local-set-key (kbd "C-x e") 'recompile)))

(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook 'smartparens-strict-mode)
(add-hook 'go-mode-hook 'my-go-hook)
(add-hook 'before-save-hook 'gofmt-before-save)

(with-eval-after-load 'go-mode
  (progn
    (message "My init :: go-mode loaded")))

(provide 'go-config)
;;; go-config.el ends here
