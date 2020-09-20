;;; lsp-settings.el --- LSP

;;; Commentary:
;; LSP configuration

;;; Code:

(setq
 lsp-idle-delay 0.500
 lsp-print-performance t
 lsp-diagnostic-package :none

 lsp-modeline-code-actions-enable nil
 lsp-ui-doc-header nil
 lsp-ui-doc-show-with-cursor nil
 lsp-ui-doc-include-signature t
 lsp-ui-doc-position 'at-point
 lsp-ui-doc-border "black")

(defun my-lsp-disable-ui-modes ()
  "Disable sideline info when lsp-ui-mode is enabled."
  (lsp-ui-sideline-mode -1)
  (lsp-ui-doc-mode -1))

(with-eval-after-load 'lsp-mode
  (progn
    (message "My init :: lsp loaded")

    (add-hook 'lsp-ui-mode-hook 'my-lsp-disable-ui-modes)

    (lsp-ui-mode)
    (lsp-diagnostics-mode -1)
    (lsp-modeline-diagnostics-mode -1)

    (define-key lsp-mode-map (kbd "s-l i") 'lsp-ivy-workspace-symbol)
    (define-key lsp-mode-map (kbd "s-l d") 'lsp-ui-doc-show)))


(provide 'lsp-settings)
;;; lsp-settings.el ends here
