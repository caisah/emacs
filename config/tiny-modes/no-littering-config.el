;;; litter.el --- litter
;;; Commentary:
;;  Configure no-littering & other dirs

;;; Code:
;; Use no-littering package
(straight-use-package 'no-littering)

;; Set .litter as the default dir for custom emacs files
(setq-default
 no-littering-etc-directory (expand-file-name ".litter/etc/" user-emacs-directory)
 no-littering-var-directory (expand-file-name ".litter/var/" user-emacs-directory)
 yas-snippet-dirs '("~/.emacs.d/snippets"))

;; Set backup dir
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
      ;; Set the temp dir
      temporary-file-directory "~/.emacs.d/.litter/temp"
      ;; Set custom file to emacs-custom.el
      custom-file (expand-file-name "config/emacs-custom.el" user-emacs-directory))


(require 'no-littering)

;; (no-littering-theme-backups)

(provide 'no-littering-config)
;;; no-littering-config.el ends here
