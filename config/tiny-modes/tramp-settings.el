;;; tramp-settings.el --- config for tramp

;;; Commentary:
;;  Tramp config

;;; Code:
(require 'tramp)

(setq-default tramp-default-method "ssh"
	      tramp-verbose 9
	      tramp-backup-directory-alist backup-directory-alist)

;; easily sudo with /sudo:root@remote-host:<path-to-file>
(add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist '((regexp-quote (system-name)) nil nil))

(with-eval-after-load 'tramp
  (progn
    (message "My init :: tramp loaded")))

(provide 'tramp-settings)
;;; tramp-settings ends here
