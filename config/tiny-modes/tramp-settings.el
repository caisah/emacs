;;; tramp-settings.el --- config for tramp

;;; Commentary:
;;  Tramp config

;;; Code:
(setq-default tramp-auto-save-directory "~/.emacs.d/cache"
              tramp-backup-directory-alist "~/.emacs.d/cache"
              tramp-persistency-file-name "tramp-persistency.el"
              tramp-default-method "ssh")

;; easily sudo with /sudo:root@remote-host:<path-to-file>
(add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist '((regexp-quote (system-name)) nil nil))

(with-eval-after-load 'tramp
  (progn
    (message "tramp loaded")))



(provide 'tramp-settings)
;;; tramp-settings ends here
