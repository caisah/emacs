;;; epa-settings.el --- config for epa

;;; Commentary:
;;  EPA

;;; Code:
;; Use gpg2 by default
(setq epg-gpg-program "gpg2")

(with-eval-after-load 'epa
  (progn
    ;; can export to text
    (setq epa-armor t)))


(provide 'epa-settings)
;;; epa-settings.el ends here
