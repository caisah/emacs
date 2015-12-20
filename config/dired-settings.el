;;; dired-settings.el --- Dired settings
;;; Commentary:
;; Dired

;;; Code:
(require 'dired)
(require 'dired-aux)
;; List directories first
(setq dired-listing-switches "-la --group-directories-first")
;; Move deteleted stuff to trash
(setq delete-by-moving-to-trash t)
;; Easily copy file to the other buffer
(setq dired-dwim-target t)

;; Set zip to decompress
(add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))

(defun dired-go-up-dir ()
  "Navigates to the parent dir."
  (interactive)
  (find-alternate-file ".."))

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

(define-key dired-mode-map (kbd "^") 'dired-go-up-dir)
(define-key dired-mode-map (kbd "k") 'dired-kill-subdir)

;; Hooks
(add-hook 'dired-mode-hook 'dired-omit-mode)

;; Enable dired plus by default
(require 'dired+)

;; Export
(provide 'dired-settings)
;;; dired-settings.el ends here
