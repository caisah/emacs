(setq user-full-name "Vlad Piersec" user-mail-address "vlad.piersec@gmail.com")

;; Cask & Pallet
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
(add-to-list 'load-path "~/.emacs.d/custom-packages/")

;; Set cache folder
(setq ido-save-directory-list-file "~/.emacs.d/cache/ido-list")
(setq bookmark-default-file "~/.emacs.d/cache/bookmarks")

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
(setq make-backup-files nil)
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

;; for eww
(setq shr-color-visible-luminance-min 70)

;; Package settings
(require 'desktop-settings)
(require 'dired-settings)
(require 'flycheck-settings)
(require 'helm-settings)
(require 'magit-settings)
(require 'othermodes-settings)

;; Programming
(require 'mongo-config)
(require 'javascript-config)
(require 'html-config)
(require 'clojure-config)
(require 'elisp-config)

;; Other
(require 'my-functions)


(provide 'general-settings)