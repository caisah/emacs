;;; mac-settings.el --- loads mac specific settings

;;; Commentary:
;;  Config file specific to mac

;;; Code:
(defconst my-darwin-assoc '((m1-ls . "/opt/homebrew/opt/coreutils/libexec/gnubin/ls")
                            (m1-bash . "/opt/homebrew/bin/bash")
                            (x64-ls . "/usr/local/opt/coreutils/libexec/gnubin/ls")
                            (x64-bash . "/usr/local/bin/bash")))

;; Set mac modifiers
(setq-default
 ;; Use coreutils ls
 insert-directory-program (cdr (assoc 'm1-ls my-darwin-assoc))
 ;; use local bash
 shell-file-name (cdr (assoc 'm1-bash my-darwin-assoc))
 ;; Use command as meta
 mac-command-modifier 'meta
 ;; Use option as super
 mac-option-modifier 'super
 ;; Used to disable s-h default shortcut
 mac-pass-command-to-system nil
;; Use aspell instead of ispell
 ispell-program-name "/opt/homebrew/bin/aspell")

(provide 'mac-settings)
;;; mac-settings ends here
