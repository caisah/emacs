;;; lsp-settings.el --- LSP

;;; Commentary:
;; LSP configuration

;;; Code:
;; Use lsp-mode
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ivy)

(defun my-lsp-hook ()
  "Hook used for lsp-mode."
  (lsp-modeline-diagnostics-mode -1))

(add-hook 'my-lsp-hook 'lsp-mode-hook)

(with-eval-after-load 'lsp-mode
  (progn
    (message "My init :: lsp loaded")

    (setq-default
     lsp-idle-delay 0.500
     lsp-headerline-breadcrumb-enable nil
     lsp-print-performance t
     lsp-modeline-diagnostics-enable nil)
    (define-key lsp-mode-map (kbd "s-l i") 'lsp-ivy-workspace-symbol)))

(provide 'lsp-settings)
;;; lsp-settings.el ends here
