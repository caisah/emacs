;;; general-settings.el --- loads settings

;;; Commentary:
;; General settings loads all the other settings

;;; Code:

;; Info
(setq user-full-name "Vlad Piersec" user-mail-address "vlad.piersec@gmail.com")

;; Cask & Pallet (this adds all pacakge files to load-path)
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; Set custom file
(setq custom-file "~/.emacs.d/config/emacs-custom.el")
;; Load custom file
(load custom-file)

;; Set cache location
(setq-default ido-save-directory-list-file "~/.emacs.d/cache/ido-list"
              bookmark-default-file "~/.emacs.d/cache/bookmarks"
              keyfreq-file "~/.emacs.d/cache/keyfrequency"
              recentf-save-file "~/.emacs.d/cache/recentf"
              mc/list-file "~/.emacs.d/cache/.mc-lists.el"
              eshell-directory-name "~/.emacs.d/cache/eshell/"
              auto-save-list-file-prefix "~/.emacs.d/cache/auto-save-list/.save-"
              projectile-cache-file "~/.emacs.d/cache/projectile/projectile.cache")

;; Set temp dir
(setq temporary-file-directory "~/.emacs.d/cache/temp")

;; Set backup dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; General settings
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; More general settings
(set-language-environment "UTF-8")
(setq custom-safe-themes t)
(setq compilation-ask-about-save nil)
(setq save-abbrevs nil)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Even more settings
(fset 'yes-or-no-p 'y-or-n-p)

;; Global modes
(global-visual-line-mode t)
(projectile-global-mode)
(show-paren-mode t)

;; Start emacs server
(require 'server)
(unless (server-running-p) (server-start))

;; Disable electric indent mode
(electric-indent-mode -1)

;; Package settings

(require 'dired-settings)
(require 'flycheck-settings)
(require 'helm-settings)
(require 'magit-settings)
(require 'org-settings)
(require 'twitter-settings)
(require 'othermodes-settings)

;; Programming
(require 'javascript-config)
(require 'typescript-config)
(require 'html-config)
(require 'elisp-config)
(require 'elm-config)
(require 'other-languages-config)


;; Other
(require 'my-functions)

;; Load global keys
(require 'globalkeys)

;; Load Abbrevs
(require 'abbrevs)

;; Export:
(provide 'general-settings)
;;; general-settings ends here
