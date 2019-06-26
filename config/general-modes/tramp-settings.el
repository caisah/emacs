;;; tramp-settings.el --- Tramp

;;; Commentary:
;; Tramp

;;; Code:

(with-eval-after-load 'tramp
    (progn
      (message "tramp-mode loaded")

      (setq tramp-default-method "ssh")

      (setq enable-recursive-minibuffers t)
      ;; C-x C-f /sudo:remote-host:/file
      (add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
      (add-to-list 'tramp-default-proxies-alist
              '((regexp-quote (system-name)) nil nil))))

(require 'tramp)

(provide 'tramp-settings)

;;; tramp-settings.el ends here
