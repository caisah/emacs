;;; ace-window-settings.el --- Easily change between windows

;;; Commentary:
;;  Navigate

;;; Code:

(global-set-key (kbd "M-p") 'ace-window)

(with-eval-after-load 'ace-window
  (progn
    (message "ace-window loaded")
    ;; enable deleting, swapping, splitting
    (setq aw-dispatch-always t)
    ;; keep keys on the home row
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))


(provide 'ace-window-settings)
;;; ace-window-settings.el ends here
