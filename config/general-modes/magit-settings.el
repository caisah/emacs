;;; magit-settings.el --- Magit settings

;;; Commentary:
;; https://github.com/magit/magit

;;; Code:

;; Set global key for magit
(global-set-key (kbd "C-x m") 'magit-status)


(with-eval-after-load 'magit
  (progn
    (message "magit loaded")

    (setq-default magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)))

(provide 'magit-settings)
;;; magit-settings.el ends here
