;;; general-settings.el --- loads all settings

;;; Commentary:
;;  General settings loads all the other specific settings & set variables

;;; Code:

;; Cask & Pallet
(if (eql system-type 'darwin)
    (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
  (require 'cask "~/.cask/cask.el"))
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; Don't log bash profile warning
(setq-default exec-path-from-shell-check-startup-files nil)
;; Set exec-path as $PATH
(exec-path-from-shell-initialize)
;; MAC
(when (eql system-type 'darwin)
  (progn
    (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls")
    (setq shell-file-name "/usr/local/bin/bash")
    ;; add node to the path
    (exec-path-from-shell-copy-env "NVM_DIR")))


;; Files created by Emacs
;; Set custom file to emacs-custom.el
(setq custom-file "~/.emacs.d/config/emacs-custom.el")
;; Set temp dir
(setq temporary-file-directory "~/.emacs.d/cache/temp")
;; Set backup dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; Set cache file names

(setq-default bookmark-default-file "~/.emacs.d/cache/bookmarks"
              keyfreq-file "~/.emacs.d/cache/keyfrequency"
              recentf-save-file "~/.emacs.d/cache/recentf"
              mc/list-file "~/.emacs.d/cache/.mc-lists.el"
              eshell-directory-name "~/.emacs.d/cache/eshell/"
              auto-save-list-file-prefix "~/.emacs.d/cache/auto-save-list/.save-"
              projectile-cache-file "~/.emacs.d/cache/projectile/projectile.cache"
              url-configuration-directory "~/.emacs.d/cache/url/"
              eww-bookmarks-directory "~/.emacs.d/cache/"
              projectile-known-projects-file "~/.emacs.d/cache/projectile/known-projects.eld")

;; Calendar
(setq-default calendar-latitude 46.7667
              calendar-longitude 23.5833)

;; Consider all themes safe
(setq-default custom-safe-themes t
              ;; don't ask to save when copiling
              compilation-ask-about-save nil
              ;; don't save abbrevs
              save-abbrevs nil
              ;; don't insert tabs on indent
              indent-tabs-mode nil
              ;; start emacs fullscreen and maximized
              initial-frame-alist (quote ((fullscreen . maximized)))
              ;; don't use angle brackets
              uniquify-buffer-name-style 'post-forward
              ;; don't put anything is *scratch*
              initial-scratch-message ""
              ;; highlight trailing spaces & tabs
              whitespace-style '(face trailing indentation::tabs)
              ;; highlight if more than 180 chars on line
              whitespace-line-column 180)

;; set language for time to Eng
(setq system-time-locale "C")

;; use UTF-8
(set-language-environment "UTF-8")


;; Load custom file
(load custom-file)

;; Start server - Now we can open any file with emacsclient
(unless (and (fboundp 'server-running-p)
             (server-running-p))
  (server-start))

;; Enable default disabled stuff
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; Use y and n as confirmations
(fset 'yes-or-no-p 'y-or-n-p)

;; clean whitespaces before saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; display startup time
(add-hook 'window-setup-hook 'my-show-startup-time)

;; Improve navigation & editing
(defun my-kill-other-buffer ()
  "Kill buffer in the other window."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window 1))

(defun my-kill-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc
   (lambda (buffer)
     (kill-buffer buffer))
   (buffer-list)))

(defun my-delete-to-previous-line ()
  "Delete to previous end of line."
  (interactive)
  (delete-horizontal-space)
  (backward-delete-char 1)
  (delete-horizontal-space)
  (insert-char 32))

(defun buffer-too-big-p ()
  "Check if a buffer is really big."
  (or (> (buffer-size) (* 5000 80))
      (> (line-number-at-pos (point-max)) 5000)))

;; Disable `linum-mode' & `flycheck-mode' for big buffers
(add-hook 'prog-mode-hook
          (lambda ()
            (if (buffer-too-big-p)
                (progn
                  (linum-mode -1)
                  (flycheck-mode -1)))))

;; Global common keys
(global-set-key (kbd "C-x K") 'my-kill-other-buffer)
(global-set-key (kbd "C-x C-k") 'my-kill-all-buffers)
(global-set-key (kbd "C-\\") 'my-delete-to-previous-line)
(global-set-key (kbd "C-S-d") 'delete-region)

(global-set-key (kbd "<C-tab>") 'indent-relative)

(global-set-key (kbd "C-c C-f") 'hs-toggle-hiding)

;; Emacs
(global-set-key (kbd "C-h C-s") 'elisp-index-search)

;; Widow resizing
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "<f1>")
                '(lambda () (interactive) (switch-to-buffer "*Messages*")))

(global-set-key (kbd "C-`") 'er/expand-region)

;; Searching
(global-set-key (kbd "C-s") nil)
(global-set-key (kbd "C-S-s") nil)
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "C-S-s") 'query-replace)


;; Global Modes:
;; Use word-wrapping for continuation lines
(global-visual-line-mode t)
;; Show matching parens
(show-paren-mode t)
;; Disable electric indent mode
(electric-indent-mode -1)
;; Save undos in tree
(global-undo-tree-mode)
;; Show info about search
(global-anzu-mode t)
;; Show beautified page breaks
(global-page-break-lines-mode t)
;; Revert buffer when the file changes on disk
(global-auto-revert-mode 1)
;; Delete selected text when starting to type
(delete-selection-mode 1)

;; required for "true" init file timing
(require 'my-functions)

;; Configure all the other modes
(require 'dired-settings)
(require 'desktop-settings)
(require 'projectile-settings)
(require 'hippie-expand-settings)
(require 'epa-settings)
(require 'company-settings)
(require 'shell-settings)
(require 'magit-settings)
(require 'helm-settings)
(require 'docview-settings)
(require 'ace-window-settings)
(require 'sml-settings)
(require 'eww-settings)
(require 'smartparens-settings)
(require 'org-settings)
(require 'mc-settings)
(require 'tramp-settings)
(require 'calendar-settings)
;; (require 'twitter-settings)
;; (require 'eww-settings)
(require 'erc-settings)
(require 'othermodes-settings)
(require 'elfeed-settings)

;; Programming
(require 'javascript-config)
(require 'web-config)
(require 'go-config)
(require 'sh-config)
(require 'rust-config)
(require 'elisp-config)
(require 'elm-config)
(require 'typescript-config)
(require 'haskell-config)
(require 'reason-config)
;; (require 'ocaml-config)
;; (require 'other-languages-config)

(require 'blog-settings)

;; Load Abbrevs
;; (require 'abbrevs)

(provide 'general-settings)
;;; general-settings ends here
