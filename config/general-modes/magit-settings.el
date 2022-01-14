;;; magit-settings.el --- Magit settings

;;; Commentary:
;; https://github.com/magit/magit

;;; Code:

;; Set global key for magit
(global-set-key (kbd "C-x m") 'magit-status)

(with-eval-after-load 'magit
  (progn
    (message "My init :: magit loaded")

    (define-key magit-mode-map (kbd "C-c a") 'magit-commit-amend)

    (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
          transient-default-level 5)))

(defun my-make-left-fringe-wider ()
  "Make left frige 20px."
  (setq left-fringe-width 20
        right-fringe-width 0))

(add-hook 'magit-mode-hook 'my-make-left-fringe-wider)

(provide 'magit-settings)
;;; magit-settings.el ends here
