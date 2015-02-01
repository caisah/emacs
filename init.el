;; The time when Emacs starts
(defvar *start-time* (current-time))

;; Add config and custom packages
(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/config/programming")
(add-to-list 'load-path "~/.emacs.d/custom-packages/")

;; Load general settings
(require 'general-settings)

;; Add theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Load Theme
(load-theme 'liso t)
