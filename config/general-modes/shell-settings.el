;;; shell-settings.el --- Shell

;;; Commentary:
;; Settings for M-x shell

;;; Code:
(with-eval-after-load 'shell
  (progn
    (message "shell-mode loaded")

    (define-key shell-mode-map (kbd "C-x l") 'rename-buffer)

    ;; strip ugly characters whe working in shell
    (add-to-list
     'comint-preoutput-filter-functions
     (lambda (output)
       (replace-regexp-in-string "\\[\\??[0-9]+[GhK]" "" output)))))

(defun my-shell-hook ()
  "Personal hook for shell mode."
  (progn
    (setq-local company-backends
                '(company-capf
                  company-files))))

(add-hook 'shell-mode-hook 'my-shell-hook)
;; show full width lines in shell mode
(add-hook 'shell-mode-hook '(lambda ()
                              (visual-line-mode nil)
                              (setq truncate-lines t)))
(add-hook 'shell-mode-hook 'company-mode)

;; opening shells
(defun my-shell-here ()
  "Open shell buffer in this window."
  (interactive)
  (let* ((dir (or dired-directory default-directory))
         (name (car (last (split-string dir "/") 2))))
   (shell (switch-to-buffer
           (generate-new-buffer (concat "*shell " name "*"))))))

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
