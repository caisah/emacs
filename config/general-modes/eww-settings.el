;;; eww-settings.el --- EWW settings

;;; Commentary:
;; Browsing the internet from within Emacs
;;; Code:
(require 'eww)

(setq-default eww-search-prefix "https://duckduckgo.com/lite?q=")
(setq-default shr-color-visible-luminance-min 77)

(define-key eww-mode-map (kbd "n") 'shr-next-link)
(define-key eww-mode-map (kbd "p") 'shr-previous-link)

(defun my-eww-open-link-at-point ()
  "Open link at cursor point in eww."
  (interactive)
  (eww (thing-at-point 'url)))

(defun my-eww-search-buffer ()
  "Start eww and add the keywords to the buffer name."
  (interactive)
  (let ((input (read-from-minibuffer "Keyword: ")))
    (progn
        (eww input)
        (rename-buffer (concat "*eww " input "*")))))

;; Export:
(provide 'eww-settings)
;;; eww-settings ends here
