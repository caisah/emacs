			   ;; Personal info

(setq user-full-name "Vlad Piersec"
      user-mail-address "vlad.piersec@gmail.com")


		     ;; Initialization and Paths:

;; Cask & Pallet
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;; Add custom packages dir to path
(add-to-list 'load-path "~/.emacs.d/custom-packages/")

;; Add themes dir to path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Set dir for Smex
(setq smex-save-file "~/.emacs.d/cache/smex-items")

;; Set dir for IDO
(setq ido-save-directory-list-file "~/.emacs.d/cache/ido-list")

;; Set dir for Bookmarks
(setq bookmark-default-file "~/.emacs.d/cache/bookmarks")

;; Set desktop variables
(setq desktop-dirname "~/.emacs.d/cache")
(setq desktop-path (list desktop-dirname))
(setq desktop-base-file-name "emacs-desktop")

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Desktop mode (save buffers on exit)
;; Autosave buffers
(require 'desktop)
(desktop-save-mode 1)

(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))

;; Save desktop on exit
(add-hook 'auto-save-hook 'my-desktop-save)

;; Package Archive
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

;; Enable
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; Set
;; Automatically save buffers before compiling
(setq compilation-ask-about-save nil)

;; Don't make ~ files
(setq make-backup-files nil)

;; Hippie Expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))


			      ;; Modes:


;; Winner mode:

(winner-mode t)

;; Dired Modes:

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
 ))

;; Sort by different criterion
(defun dired-sort-criteria (criteria)
  "sort-dired by different criteria by Robert Gloeckner "
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

;; Ibuffer:

(autoload 'ibuffer "ibuffer" "List buffers." t)
;; Get rid of title and summary
(defadvice ibuffer-update-title-and-summary (after remove-column-titles)
  (save-excursion
    (set-buffer "*Ibuffer*")
    (toggle-read-only 0)
    (goto-char 1)
    (search-forward "-\n" nil t)
    (delete-region 1 (point))
    (let ((window-min-height 1)) 
      ;; save a little screen estate
      (shrink-window-if-larger-than-buffer))
    (toggle-read-only)))

(ad-activate 'ibuffer-update-title-and-summary)

;; IDO related:


(require 'ido)
(ido-mode t)

;; IDO vertical 
 (require 'ido-vertical-mode)
 (ido-vertical-mode 1)

;; Enable Yes or No
(ido-yes-or-no-mode t)

;; IDO ubiquitous
(require 'ido-ubiquitous)
(ido-ubiquitous t)

;; IDO flx
(require 'flx-ido)
(flx-ido-mode t)

;; Smex
(require 'smex)
(smex-initialize)

;; SmartParens:

(require 'smartparens-config)
(require 'smartparens-html)
(smartparens-global-mode t)

;; Abbrev mode:

;; stop asking whether to save newly added abbrev when quitting 
(setq save-abbrevs nil)

;; Smart mode line (hide modes in bar)

(require 'smart-mode-line)
(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))
(setq sml/hidden-modes (list " Anzu" " ARev" " SP/s" " SP" " Abbrev" " Isearch"
			     " A" " Guide"  " Undo-Tree" " PgLn" " MRev"
			     " skewer" " skewer-html" " skewer-css"" Emmet" " hs"
			     ))

;; Move region
(require 'move-text)

(move-text-default-bindings)

;; Move region

(require 'multiple-cursors)

;; Guide Key

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x 5" "C-x n"))
(guide-key-mode 1) ; Enable guide-key-mode
(setq guide-key/popup-window-position 'left)

;; Markdown mode

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Httpd

(require 'simple-httpd)
(setq httpd-root "~/Documents/javascript/")

;; Smart Compile

(require 'smart-compile)

;; Kill Ring

(require 'kill-ring-ido)
;; Change maximum length
(setq kill-ring-ido-shortage-length 60)

;; Undo Tree

(require 'undo-tree)
(global-undo-tree-mode)

;; Expand region:

(require 'expand-region)

;; Anzu (search)

(require 'anzu)
(global-anzu-mode t)

;; Tramp:

(require 'tramp)
(setq tramp-default-method "ssh")

;; Rainbow delimiters:

(require 'rainbow-delimiters)

;; Pretty lambda:

(require 'pretty-lambdada)

;; Re-builder

(require 're-builder)
(setq reb-re-syntax 'string)

;; Page Break Lines

(require 'page-break-lines)
(global-page-break-lines-mode t)


;; Google Translate

(require 'google-translate)
(require 'google-translate-smooth-ui)
(global-set-key "\C-cT" 'google-translate-smooth-translate)
(global-set-key "\C-ct" 'google-translate-at-point)
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "ro")
(setq google-translate-translation-directions-alist
      '(("en" . "ro") ("ro" . "en")))
(setq google-translate-enable-ido-completion t)
(setq google-translate-show-phonetic t)

;; Emmet

(require 'emmet-mode)
(setq emmet-preview-default nil)

;; Node

(require 'js-comint)
(setq inferior-js-program-command "nodejs")
(setenv "NODE_NO_READLINE" "1")

;; Imenu
(global-set-key (kbd "C-.") 'imenu-anywhere)
