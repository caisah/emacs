;;; tramp-settings.el --- Dired

;;; Commentary:
;; Tramp

;;; Code:

(with-eval-after-load 'tramp
    (progn
      (message "tramp-mode loaded")
      ;; C-x C-f /sudo:remote-host:/file
      (add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
      (add-to-list 'tramp-default-proxies-alist
              '((regexp-quote (system-name)) nil nil))))

(provide 'tramp-settings)

;;; tramp-settings.el ends here
