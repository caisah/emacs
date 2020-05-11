;;; ibuffer-settings.el --- Ibuffer

;;; Commentary:
;; Manage buffers

;;; Code:

(defun do-not-truncate-lines ()
  "Lines will not be wrapped."
  (toggle-truncate-lines 1))

(setq-default
 ;; Set modified char to 'm'
 ibuffer-modified-char 109
 ;; Set marked char to '*'
 ibuffer-marked-char 42
 ;; Set read only char to 'r'
 ibuffer-read-only-char 114
 ;; Sort by major mode
 ibuffer-default-sorting-mode 'major-mode)

;; Customize colors for buffer names
(setq-default ibuffer-fontification-alist
  '((10 (and buffer-file-name
             (buffer-modified-p (get-file-buffer buffer-file-name))) ibuffer-modified-face)
    (20 buffer-read-only ibuffer-read-only-face)
    (30 (string-match "^\\*" (buffer-name)) ibuffer-emacs-buffer-face)
    (35 (derived-mode-p 'dired-mode) font-lock-function-name-face)
    (40 (and (boundp 'emacs-lock-mode) emacs-lock-mode) ibuffer-locked-buffer)))

(with-eval-after-load 'ibuffer
  (progn
    (message "My init :: ibuffer loaded")

    (do-not-truncate-lines)
    (add-hook 'ibuffer-hook 'do-not-truncate-lines)))

(provide 'ibuffer-settings)
;;; ibuffer-settings.el ends here
