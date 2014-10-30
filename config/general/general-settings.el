(setq user-full-name "Vlad Piersec" user-mail-address "vlad.piersec@gmail.com")

;; Cask & Pallet
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
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

;; Package settings
(require 'desktop-settings)
(require 'dired-settings)
(require 'flycheck-config)
(require 'ibuffer-settings)
(require 'magit-settings)
(require 'markdown-settings)
(require 'smartparens-settings)
(require 'sml-settings)
(require 'tramp-settings)
(require 'projectile-settings)
(require 'yas-settings)
(require 'org-settings)
(require 'helm-settings)
(require 'othermodes-settings)
(require 'calendar-settings)
;; Other settings
(require 'programming-settings)
(require 'snippets)


(provide 'general-settings)



