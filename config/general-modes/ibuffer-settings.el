;;; ibuffer-settings.el --- Ibuffer

;;; Commentary:
;; Manage buffers

;;; Code:

(defun do-not-truncate-lines ()
  "Lines will not be wrapped."
  (toggle-truncate-lines 1))

(with-eval-after-load 'ibuffer
  (progn
    (message "My init :: ibuffer loaded")

    (add-hook 'ibuffer-hook 'do-not-truncate-lines)))


(provide 'ibuffer-settings)

;;; ibuffer-settings.el ends here
