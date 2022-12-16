;;; linux-settings.el --- loads linux specific settings

;;; Commentary:
;;  Config file specific to linux
;;; Code:

;; Set linux modifiers
(setq-default
 ;; Use coreutils ls
 insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls"
 ;; use local bash
 shell-file-name "/usr/local/bin/bash")

(provide 'linux-settings)
;;; linux-settings.el ends here
