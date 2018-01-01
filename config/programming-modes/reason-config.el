;;; reason-config.el --- ReasonML config

;;; Commentary:
;; My reasonML config file

;;; Code:
(defun shell-cmd (cmd)
  "Returns the stdout output of a shell command or nil if the command returned an error."
  (car (ignore-errors (apply 'process-lines (split-string cmd)))))

(defun my-reason-hook ()
  "My reason mode hook."
  (progn
    (add-hook 'before-save-hook 'refmt-before-save)
    (merlin-mode)))

(add-hook 'reason-mode-hook 'company-mode)
(add-hook 'reason-mode-hook 'linum-mode)
(add-hook 'reason-mode-hook 'smartparens-strict-mode)
(add-hook 'reason-mode-hook 'my-reason-hook)

(with-eval-after-load 'reason-mode
  (progn
    (message "reason-mode loaded")

    (require 'merlin)

    (setq-default refmt-command (shell-cmd "which refmt"))
    (setq-default merlin-command (shell-cmd "which ocamlmerlin"))
    (setq-default merlin-ac-setup t)))

(provide 'reason-config)
;;; reason-config.el ends here
