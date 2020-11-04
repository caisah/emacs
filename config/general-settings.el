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
    ;; Use command as super
    (setq mac-command-modifier 'super)
    ;; Use option as meta
    (setq mac-option-modifier 'meta)
    ;; Used to disable s-h default shortcut
    (setq mac-pass-command-to-system nil)
    (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls")
    (setq shell-file-name "/usr/local/bin/bash")))

;; Set gc limit
(setq gc-cons-threshold (* 40 1024 1024))
;; Enter debugger if an error is signaled.
(setq debug-on-error t)
;; add node to the path
(exec-path-from-shell-copy-env "NVM_DIR")


;; Files created by Emacs
;; Set .litter as the default dir for custom emacs files
(setq no-littering-etc-directory
      (expand-file-name ".litter/etc/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name ".litter/var/" user-emacs-directory))
(require 'no-littering)
;; Set backup dir
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
;; Set the temp dir
(setq temporary-file-directory "~/.emacs.d/.litter/temp")
;; Set custom file to emacs-custom.el
(setq custom-file (expand-file-name "config/emacs-custom.el" user-emacs-directory))
;; Make buffer names unique
(setq uniquify-buffer-name-style 'post-forward)
;; Don't create lock files
(setq create-lockfiles nil)

;; Calendar
(setq calendar-latitude 46.7667
      calendar-longitude 23.5833)

;; Consider all themes safe
(setq custom-safe-themes t
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
(defun my-buffer-too-big-p ()
  "Check if a buffer is really big."
  (or (> (buffer-size) (* 5000 80))
      (> (line-number-at-pos (point-max)) 5000)))

;; Disable `flycheck-mode' for big buffers
(add-hook 'prog-mode-hook
          (lambda ()
            (if (my-buffer-too-big-p)
                (progn
                  (flycheck-mode -1)))))

;; Global common keys
(global-set-key (kbd "C-x K") 'my-kill-other-buffer)
(global-set-key (kbd "C-x C-k") 'my-kill-all-buffers)
(global-set-key (kbd "C-\\") 'my-delete-to-previous-line)
(global-set-key (kbd "C-S-d") 'delete-region)
(global-set-key (kbd "C-x M-w") 'my-copy-buffer-file-name)
(global-set-key (kbd "C-x i") 'ibuffer)

(global-set-key (kbd "<C-tab>") 'indent-relative)

(global-set-key (kbd "C-c C-f") 'hs-toggle-hiding)
(global-set-key (kbd "s-u") 'revert-buffer)

;; Emacs
(global-set-key (kbd "C-h C-s") 'elisp-index-search)

;; Widow resizing
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "<f1>")
                (lambda () (interactive) (switch-to-buffer "*Messages*")))

(global-set-key (kbd "C-`") 'er/expand-region)

(global-set-key (kbd "C-S-s") 'anzu-query-replace-regexp)


;; Global Modes:
;; Use word-wrapping for continuation lines
(global-visual-line-mode t)
;; Show matching parens
(show-paren-mode t)
;; Disable electric indent mode
(electric-indent-mode -1)
;; Show info about search
(global-anzu-mode t)
;; Show beautified page breaks
(global-page-break-lines-mode t)
;; Revert buffer when the file changes on disk
(global-auto-revert-mode 1)
;; Delete selected text when starting to type
(delete-selection-mode 1)
;; Save state between sessions
(desktop-save-mode t)
(setq desktop-save t)

;; Visual Stuff
;;
;; Disable menu bar mode
(menu-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable scroll
(set-scroll-bar-mode nil)

;; Fringes
(set-fringe-mode '(8 . 0))

;; Show size of file
(size-indication-mode t)

;; Show column number
(column-number-mode t)

;; required for "true" init file timing
(require 'my-functions)

;; Configure all the other modes
(require 'dired-settings)
(require 'ibuffer-settings)
(require 'projectile-settings)
(require 'hippie-expand-settings)
(require 'epa-settings)
(require 'company-settings)
(require 'shell-settings)
(require 'magit-settings)
(require 'ivy-settings)
(require 'docview-settings)
(require 'ace-window-settings)
(require 'sml-settings)
(require 'eww-settings)
(require 'smartparens-settings)
(require 'org-settings)
(require 'mc-settings)
(require 'tramp-settings)
(require 'calendar-settings)
(require 'eww-settings)
(require 'erc-settings)
(require 'lsp-settings)
(require 'othermodes-settings)

;; Programming
(require 'javascript-config)
(require 'web-config)
(require 'sh-config)
(require 'elisp-config)
(require 'elm-config)
(require 'typescript-config)
(require 'reason-config)
(require 'go-config)
;; (require 'ocaml-config)
;; (require 'other-languages-config)

(require 'blog-settings)

;; Load Abbrevs
;; (require 'abbrevs)

(provide 'general-settings)
;;; general-settings ends here

