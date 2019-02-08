;;; magit-settings.el --- Magit settings

;;; Commentary:
;; https://github.com/magit/magit

;;; Code:

;; Set global key for magit
(global-set-key (kbd "C-x m") 'magit-status)


(with-eval-after-load 'magit
  (progn
    (message "magit loaded")

    (setq-default magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
    ;; Unbind C-x M-g as is used for helm-ag
    (define-key magit-file-mode-map (kbd "C-x M-g") nil)))

(add-hook 'magit-mode-hook 'my-make-left-fringe-wider)

(provide 'magit-settings)
;;; magit-settings.el ends here
