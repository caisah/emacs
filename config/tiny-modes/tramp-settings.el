;;; tramp-settings.el --- config for tramp

;;; Commentary:
;;  Tramp config

;;; Code:
(with-eval-after-load 'tramp
  (progn
    (message "tramp loaded")

    (setq-default  tramp-default-method "ssh")
    ;; easily sudo with /sudo:root@remote-host:<path-to-file>
    (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
    (add-to-list 'tramp-default-proxies-alist '((regexp-quote (system-name)) nil nil))))


(provide 'tramp-settings)
;;; tramp-settings ends here
