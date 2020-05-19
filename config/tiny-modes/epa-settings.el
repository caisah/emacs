;;; epa-settings.el --- config for epa

;;; Commentary:
;;  EPA

;;; Code:
(with-eval-after-load 'epa
  (progn
    "My init :: epa loaded"
    ;; can export to text
    (setq epa-armor t
	  ;; Use gpg2 by default
	  epg-gpg-program "gpg2")))

(provide 'epa-settings)
;;; epa-settings.el ends here
