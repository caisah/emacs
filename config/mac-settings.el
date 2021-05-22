;;; mac-settings.el --- loads mac specific settings

;;; Commentary:
;;  Config file specific to mac

;;; Code:
(defconst my-darwin-assoc '((m1-cask . "/opt/homebrew/share/emacs/site-lisp/cask/cask.el")
                            (m1-ls . "/opt/homebrew/opt/coreutils/libexec/gnubin/ls")
                            (m1-bash . "/opt/homebrew/bin/bash")
                            (x64-cask . "/usr/local/share/emacs/site-lisp/cask/cask.el")
                            (x64-ls . "/usr/local/opt/coreutils/libexec/gnubin/ls")
                            (x64-bash . "/usr/local/bin/bash")))

;; use specific files based on cask location
(if (file-exists-p (cdr (assoc 'm1-cask my-darwin-assoc)))
    (progn
      (require 'cask (cdr (assoc 'm1-cask my-darwin-assoc)))
      (setq-default
       insert-directory-program (cdr (assoc 'm1-ls my-darwin-assoc))
       shell-file-name (cdr (assoc 'm1-bash my-darwin-assoc))))
  (progn
    (require 'cask (cdr (assoc 'x64-cask my-darwin-assoc)))
    (setq-default
     insert-directory-program (cdr (assoc 'x64-ls my-darwin-assoc))
     shell-file-name (cdr (assoc 'x64-bash my-darwin-assoc)))))

;; Set mac modifiers
(setq-default
 ;; Use command as meta
 mac-command-modifier 'meta
 ;; Use option as super
 mac-option-modifier 'super
 ;; Used to disable s-h default shortcut
 mac-pass-command-to-system nil)


(provide 'mac-settings)
;;; mac-settings ends here
