;;; sml-settings.el --- Smart-Mode-Line

;;; Commentary:
;; Settings for smart-mode-line

;;; Code:
(require 'smart-mode-line)

(setq-default
 ;; don't ask for a confirmation when loading sml theme
 sml/no-confirm-load-theme t
 ;; use default theme (it's customized in theme anyway)
 sml/theme 'automatic
 ;; max 15 chars
 sml/mode-width 15
 ;; don't show these modes in mode-line
 rm-whitelist (mapconcat 'identity '("FlyC") "\\|"))

(sml/setup)


(provide 'sml-settings)
;;; sml-settings ends here
