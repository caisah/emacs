;; Company
(setq company-tern-meta-as-single-line t)
(setq company-tooltip-align-annotations t)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-n") #'company-select-previous))


(provide 'company-settings)
