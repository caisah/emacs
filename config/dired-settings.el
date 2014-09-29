;; List directories first
(setq dired-listing-switches "-la --group-directories-first")

;; KEY 
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "^")
	      (lambda () (interactive) (find-alternate-file "..")))))

;; Sort by different criterion
(defun dired-sort-criteria (criteria)
  "Sort-dired by different criteria by Robert Gloeckner "
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

;; Enable dired-details
(require 'dired-details)
(dired-details-install)
(add-hook 'dired-mode-hook 'dired-details-hide)

;; Enable dired plus by default
(require 'dired+)

(provide 'dired-settings)
