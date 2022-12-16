;;; company-settings.el --- config for hippie expand

;;; Commentary:
;;  Company mode config

;; Use company package
(straight-use-package 'company)

;; Use company-quickhelp package
(straight-use-package 'company-quickhelp)

(with-eval-after-load 'company
  (progn
    (message "My init :: company-mode loaded")
    ;; let tab do what it does best
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-mode-map (kbd "C-o") 'company-capf)))


(with-eval-after-load 'company-quickhelp
  (progn
    (message "My init :: company-quickhelp loaded")
    ;; show tooltip popup on key press
    (setq company-quickhelp-delay nil)
    (define-key company-active-map (kbd "C-j") 'company-quickhelp-manual-begin)))

;; show documentation when using company
(add-hook 'company-mode-hook 'company-quickhelp-mode)


(provide 'company-settings)
;;; company-settings.el ends here
