;;; dired-settings.el --- Dired settings

;;; Commentary:
;; Dired

;;; Code:
;; List directories first
(setq dired-listing-switches "-la --group-directories-first")

;; KEY
(add-hook 'dired-mode-hook
          (lambda () (define-key dired-mode-map (kbd "^")
                  (lambda () (interactive) (find-alternate-file "..")))))


;; Sort by different criterion
(defun dired-sort-criteria (criteria)
  "Sort-dired by different CRITERIA."
  (interactive
   (list
    (or (completing-read "criteria [name]: "
                         '("size(S)" "extension(X)" "creation-time(ct)"
                           "access-time(ut)" "time(t)" "name()"))
        "")))
  (string-match ".*(\\(.*\\))" criteria)
  (dired-sort-other
   (concat dired-listing-switches
           (match-string 1 criteria))))

;; Enable dired plus by default
(require 'dired+)

;; Export
(provide 'dired-settings)
;;; dired-settings.el ends here
