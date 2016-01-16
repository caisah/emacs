;;; eww-settings.el --- EWW settings

;;: Commentary:
;; Browsing the internet from within emacs
;;; Code:
(require 'eww)

(setq-default eww-search-prefix "https://google.com/search?q=")
(setq-default shr-color-visible-luminance-min 77)

(define-key eww-mode-map (kbd "n") 'shr-next-link)
(define-key eww-mode-map (kbd "p") 'shr-previous-link)

(defun eww-open-link-at-point ()
  (interactive)
  (eww (thing-at-point 'url)))

;; Export:
(provide 'eww-settings)
;;; eww-settings ends here
