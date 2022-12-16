;;; exec-path-from-shell.el --- load shell variabls
;;; Commentary:
;;  Use exect-path-from-shell package to load all shell variables

;;; Code:

;; Use exec-path-from-shell package
(straight-use-package 'exec-path-from-shell)
;; add node to the path
(exec-path-from-shell-copy-env "NVM_DIR")
;; Don't log bash profile warning
(setq-default exec-path-from-shell-check-startup-files nil)
;; Set exec-path as $PATH
(exec-path-from-shell-initialize)

(provide 'exec-path)
;;; exec-path.el ends here
