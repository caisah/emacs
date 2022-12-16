;;; mac-settings.el --- loads mac specific settings

;;; Commentary:
;;  Config file specific to mac

;; Set mac modifiers
;;; Code:
(setq-default
 ;; Use coreutils ls
 insert-directory-program "/opt/homebrew/opt/coreutils/libexec/gnubin/ls"
 ;; use local bash
 shell-file-name "/opt/homebrew/bin/bash"
 ;; Use command as meta
 mac-command-modifier 'meta
 ;; Use option as super
 mac-option-modifier 'super
 ;; Used to disable s-h default shortcut
 mac-pass-command-to-system nil
;; Use aspell instead of ispell
 ispell-program-name "/opt/homebrew/bin/aspell")

(provide 'mac-settings)
;;; mac-settings.el ends here
