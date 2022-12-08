;;; dired-settings.el --- Dired

;;; Commentary:
;; Dired, dired-aux & dired+ settings

;;; Code:
(defun dired-go-up-dir ()
  "Navigates to the parent dir."
  (interactive)
  (find-alternate-file ".."))

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

(defun my-dired-kill-subdir ()
  (interactive)
  (dired-kill-subdir)
  (pop-global-mark))

(with-eval-after-load 'dired
  (progn
    (message "My init :: dired loaded")

    (require 'dired-aux)
    (require 'dired+)

    ;; Do async operations when dealing with files
    (dired-async-mode 1)

    ;; Show directories first
    (setq dired-use-ls-dired t
          dired-listing-switches "-aBhl --group-directories-first"
          ;; Don't ask for confirmation on recursion when copying
          dired-recursive-copies 'always
          ;; Don't ask for confirmation on recursion when deleting
          dired-recursive-deletes 'always
          ;; Easily copy file to the other buffer
          dired-dwim-target t)

    ;; Use unzip for .zip files
    (add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))
    ;; Use tar for .tar.xz files
    (add-to-list 'dired-compress-file-suffixes '("\.tar\.xz" ".tar" "tar xf %i"))

    ;; Disable annoying warining
    (put 'dired-find-alternate-file 'disabled nil)

    ;; Define specific keys
    (define-key dired-mode-map (kbd "^") 'dired-go-up-dir)
    (define-key dired-mode-map (kbd "k") 'my-dired-kill-subdir)

    ;; Hooks
    ;; Omit uninteresting files in Dired including .. and .
    (add-hook 'dired-mode-hook 'dired-omit-mode)
    ;; Refresh dired when file changes
    (add-hook 'dired-mode-hook 'auto-revert-mode)

    ;; Move deleted stuff to trash
    (setq delete-by-moving-to-trash t)))

(provide 'dired-settings)
;;; dired-settings.el ends here
