;;; eww-settings.el --- Browser

;;; Commentary:
;;  Visit webpages

;;; Code:
(with-eval-after-load 'eww
  (progn
    (message "eww loaded")
    ;; keep bookmarks in cache dir
    (setq-default eww-bookmarks-directory "~/.emacs.d/cache")
    ))


(provide 'eww-settings)
;;; eww-settings.el ends here
