;;; reason-config.el --- ReasonML config

;;; Commentary:
;; My reasonML config file

;;; Code:
(defun my-reason-hook ()
  "Hook for reason."
  (add-hook 'before-save-hook 'refmt-before-save))

(add-hook 'reason-mode-hook 'my-reason-hook)
(add-hook 'reason-mode-hook 'lsp-ocaml-enable)
(add-hook 'reason-mode-hook 'company-mode)
(add-hook 'reason-mode-hook 'flycheck-mode)
(add-hook 'reason-mode-hook 'linum-mode)

(with-eval-after-load 'reason-mode
  (progn
    (message "reason-mode loaded")
    (require 'lsp-mode)
    (require 'lsp-ocaml)
    (require 'lsp-flycheck)

    (setq-default merlin-ac-setup t)))

(provide 'reason-config)
;;; reason-config.el ends here
