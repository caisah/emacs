;;; ibuffer-settings.el --- Ibuffer

;;; Commentary:
;; Manage buffers

;;; Code:

(defun do-not-truncate-lines ()
  "Lines will not be wrapped."
  (toggle-truncate-lines 1))

(setq-default ibuffer-modified-char 8635)
(setq-default ibuffer-marked-char 42)
(setq-default ibuffer-read-only-char 82)
(setq-default ibuffer-default-sorting-mode 'major-mode)

(with-eval-after-load 'ibuffer
  (progn
    (message "My init :: ibuffer loaded")

    (do-not-truncate-lines)
    (add-hook 'ibuffer-hook 'do-not-truncate-lines)))


(provide 'ibuffer-settings)

;;; ibuffer-settings.el ends here
