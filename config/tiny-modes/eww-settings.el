;;; eww-settings.el --- Browser

;;; Commentary:
;;  Visit webpages

;;; Code:
(setq eww-bookmarks-directory "~/.emacs.d/cache")

(with-eval-after-load 'eww
  (progn
    (message "eww loaded")
    ;; keep bookmarks in cache dir
    ))


(provide 'eww-settings)
;;; eww-settings.el ends here
