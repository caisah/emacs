;;; sh-config.el --- Shell config

;;; Commentary:
;; My Shell script config file


;;; Code:
(add-hook 'sh-mode-hook 'flycheck-mode)
(add-hook 'sh-mode-hook 'linum-mode)

(with-eval-after-load 'sh-script
  (progn
    (message "sh-mode loaded")))

;; opening shells
(defun my-shell-here ()
  (interactive)
  (shell (switch-to-buffer
          (generate-new-buffer default-directory))))

(defun my-shell-other ()
  (interactive)
  (progn
    (other-window 1)
    (my-shell-here)))

(defun my-shell-split-other ()
  (interactive)
  (progn
    (other-window 1)
    (split-window-below)
    (my-shell-here)))

(defun my-shell-split-below ()
  (interactive)
  (progn
    (other-window 1)
    (split-window-below)
    (other-window 1))
    (my-shell-here))


(provide 'sh-config)
;;; sh-config.el ends here
