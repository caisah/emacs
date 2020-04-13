;;; ace-window-settings.el --- Easily change between windows

;;; Commentary:
;;  Navigate

;;; Code:

(global-set-key (kbd "C-c o") 'ace-window)

(with-eval-after-load 'ace-window
  (progn
    (message "My init :: ace-window loaded")
    ;; enable deleting, swapping, splitting
    (setq-default aw-dispatch-always t
                  ;; keep keys on the home row
                  aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(provide 'ace-window-settings)
;;; ace-window-settings.el ends here
