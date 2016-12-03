;;; magit-settings.el --- Magit settings

;;; Commentary:
;; https://github.com/magit/magit

;;; Code:

;; Set global key for magit
(global-set-key (kbd "C-x m") 'magit-status)


(with-eval-after-load 'magit
  (progn
    (message "magit loaded")))

(provide 'magit-settings)
;;; magit-settings.el ends here
