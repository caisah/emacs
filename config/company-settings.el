;; Company
(setq company-tern-meta-as-single-line t)
(setq company-tooltip-align-annotations t)
;; Tern
(add-to-list 'company-backends 'company-tern)
(setq company-tern-meta-as-single-line t)


(provide 'company-settings)
