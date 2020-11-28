;;; lsp-settings.el --- LSP

;;; Commentary:
;; LSP configuration

;;; Code:

(defun my-lsp-hook ()
  "Hook used for lsp-mode."
  (lsp-diagnostics-mode -1)
  (lsp-modeline-diagnostics-mode -1))

(add-hook 'my-lsp-hook 'lsp-mode-hook)

(with-eval-after-load 'lsp-mode
  (progn
    (message "My init :: lsp loaded")

    (setq-default
     lsp-idle-delay 0.500
     lsp-print-performance t
     lsp-diagnostic-package :none)
    (define-key lsp-mode-map (kbd "s-l i") 'lsp-ivy-workspace-symbol)))

(provide 'lsp-settings)
;;; lsp-settings.el ends here
