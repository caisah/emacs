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

(defun dired-delete-and-refresh (&optional arg)
  (interactive)
  (dired-do-delete arg)
  (revert-buffer))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
  (define-key dired-mode-map (kbd "I") nil)
  (define-key dired-mode-map (kbd "I") 'dired-subtree-remove)
  (define-key dired-mode-map (kbd "C-M-n") nil)
  (define-key dired-mode-map (kbd "C-M-n") 'dired-subtree-down)
  (define-key dired-mode-map (kbd "C-M-p") nil)
  (define-key dired-mode-map (kbd "C-M-p") 'dired-subtree-up)
  (define-key dired-mode-map (kbd "D") nil)
  (define-key dired-mode-map (kbd "D") 'dired-delete-and-refresh))


(setq dired-subtree-use-backgrounds nil)

(provide 'dired-settings)
