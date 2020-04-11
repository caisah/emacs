;;; shell-settings.el --- Eshell

;;; Commentary:
;; Settings for M-x eshell.

;;; Code:
(with-eval-after-load 'esh-mode
  (progn
    (message "My init :: eshell-mode loaded")

    (define-key eshell-mode-map (kbd "C-x l") 'rename-buffer)))

;; show full width lines in shell mode
(add-hook 'eshell-mode-hook '(lambda ()
                              (visual-line-mode nil)
                              (setq truncate-lines t)))
(add-hook 'eshell-mode-hook 'company-mode)

;; opening shells
(defun my-shell-here ()
  "Open shell buffer in this window."
  (interactive)
  (let* ((dir (or dired-directory default-directory))
         (name (car (last (split-string dir "/") 2))))

    (eshell "")
    (rename-buffer (concat name " *shell*"))))

(defun my-shell-other ()
  "Open shell buffer in the other window."
  (interactive)
  (progn
    (other-window 1)
    (my-shell-here)))

(defun my-shell-split-up ()
  "Split other window up and open shell buffer."
  (interactive)
  (progn
    (other-window 1)
    (split-window-below)
    (my-shell-here)))

(defun my-shell-split-below ()
  "Split other window down and open shell buffer."
  (interactive)
  (progn
    (other-window 1)
    (split-window-below)
    (other-window 1))
    (my-shell-here))

(global-set-key (kbd "C-c s") 'my-shell-here)

(provide 'shell-settings)
;;; shell-settings ends here
