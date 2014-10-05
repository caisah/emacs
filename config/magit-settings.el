(require 'magit)

(set-default 'magit-stage-all-confirm nil)
(set-default 'magit-unstage-all-confirm nil)

(defun magit-toggle-all ()
  "Toggles all sections"
  (interactive)
  (defun toggle-until-end ()
    (magit-toggle-section)
    (if (magit-goto-next-section)
        (prog1
          (beginning-of-buffer)
          (message "Toggled all"))
      (toggle-until-end)))
  (beginning-of-buffer)
  (toggle-until-end))
(define-key  magit-mode-map (kbd "T") 'magit-toggle-all)


(provide 'magit-settings)
