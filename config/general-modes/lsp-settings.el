;;; lsp-settings.el --- LSP

;;; Commentary:
;; LSP configuration

;;; Code:
(setq lsp-keymap-prefix "C-c l"
      lsp-idle-delay 0.500
      lsp-print-performance t
      lsp-diagnostic-package :none)

(with-eval-after-load 'lsp-mode
  (progn
    (message "My init :: lsp loaded")

    (define-key lsp-mode-map (kbd "C-c l i") 'lsp-ivy-workspace-symbol)))


(provide 'lsp-settings)
;;; lsp-settings.el ends here
