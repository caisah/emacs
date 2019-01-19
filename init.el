;;; init.el -- Start here

;;; Commentary:
;;  Add all dirs to the path and load settings

;;; Code:
(defvar *start-time* (current-time))

;; Enable debugger
(setq debug-on-error t)

;; Add theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Add config and custom packages
(add-to-list 'load-path "~/.emacs.d/config/blog")
(add-to-list 'load-path "~/.emacs.d/custom-packages/")
(add-to-list 'load-path "~/.emacs.d/config/programming-modes")
(add-to-list 'load-path "~/.emacs.d/config/tiny-modes")
(add-to-list 'load-path "~/.emacs.d/config/general-modes")
(add-to-list 'load-path "~/.emacs.d/config/elisp")
(add-to-list 'load-path "~/.emacs.d/config")

;; Load Theme
(load-theme 'paleolithic t)

;; Load the settings
(require 'general-settings)

;;; init.el ends here
