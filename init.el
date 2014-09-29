;; The time when Emacs starts
(defvar *start-time* (current-time))

(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/config/general")
(add-to-list 'load-path "~/.emacs.d/config/programming")
(add-to-list 'load-path "~/.emacs.d/config/src")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-file "~/.emacs.d/config/custom.el")

;; Load general settings
(require 'general-settings)

;; Load custom file
(load custom-file)

;; Load keys settings
(require 'globalkeys-settings)

;; Load Abbrevs
(require 'abbrevs)

;; Load Theme
(load-theme 'liso t)
;; ugly temporary fix until the problem is resolved
(load "~/.emacs.d/themes/liso-theme.el")
