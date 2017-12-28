;;; reason-config.el --- ReasonML config

;;; Commentary:
;; My reasonML config file

;;; Code:
(defun shell-cmd (cmd)
  "Returns the stdout output of a shell command or nil if the command returned an error."
  (car (ignore-errors (apply 'process-lines (split-string cmd)))))

(let* ((refmt-bin (or (shell-cmd "refmt ----where")
                      (shell-cmd "which refmt")))
       (merlin-bin (or (shell-cmd "ocamlmerlin ----where")
                       (shell-cmd "which ocamlmerlin")))
       (merlin-base-dir (when merlin-bin
                          (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
  ;; Add npm merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
  (when merlin-bin
    (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
    (setq-default merlin-command merlin-bin))

  (when refmt-bin
    (setq-default refmt-command refmt-bin)))

(defun my-reason-hook ()
  "Hook for reason."
  (add-hook 'before-save-hook 'refmt-before-save)
  (merlin-mode))

(add-hook 'reason-mode-hook 'my-reason-hook)
(add-hook 'reason-mode-hook 'company-mode)
(add-hook 'reason-mode-hook 'flycheck-mode)
(add-hook 'reason-mode-hook 'linum-mode)

(with-eval-after-load 'reason-mode
  (progn
    (message "reason-mode loaded")
    (require 'merlin)

    (setq-default merlin-ac-setup t)))

(provide 'reason-config)
;;; reason-config.el ends here
