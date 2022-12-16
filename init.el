;;; init.el -- Start here

;;; Commentary:
;;  Add all dirs to the path and load settings

;;; Code:
(defvar *start-time* (current-time))

(require 'cl-lib)
(require 'seq)

;; Defun my config dirs
(defconst my-config-dir "~/.emacs.d/config")
(defconst my-themes-dir "~/.emacs.d/themes")

;; Bootstrap Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Add theme
(add-to-list 'custom-theme-load-path my-themes-dir)

(defun my-subdirs-and-dir (dir)
  "Return a list containing the DIR and all subdirs of DIR."
  (cons
   dir
   (seq-filter
    'file-directory-p
    (mapcar (lambda (name)
	      (expand-file-name name dir))
	    (cddr (directory-files dir))))))

(defun my-add-to-path-dirs (dirs)
  "Add to `load-path' all DIRS and subdirectories of DIRS."
  (mapc (lambda (dir)
          (add-to-list 'load-path dir))
        (cl-reduce
         'append
         (mapcar 'my-subdirs-and-dir dirs))))

;; Add all dirs to load path
(my-add-to-path-dirs (list my-config-dir))

;; Load Theme
(load-theme 'paleolithic t)

;; Load the settings
(require 'general-settings)

;;; init.el ends here
