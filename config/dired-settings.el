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

;; Enable dired plus by default
(require 'dired+)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
  (define-key dired-mode-map (kbd "I") nil)
  (define-key dired-mode-map (kbd "I") 'dired-subtree-remove)
  (define-key dired-mode-map (kbd "C-N") nil)
  (define-key dired-mode-map (kbd "C-N") 'dired-subtree-down)
  (define-key dired-mode-map (kbd "C-P") nil)
  (define-key dired-mode-map (kbd "C-P") 'dired-subtree-up))

(provide 'dired-settings)
