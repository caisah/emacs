;;; general-settings.el --- loads all settings

;;; Commentary:
;;  General settings loads all the other specific settings & set variables

;;; Code:

;; Coreutils + bash
(if (eql system-type 'darwin)
    (require 'mac-settings))

;; Set gc limit
(setq gc-cons-threshold (* 40 1024 1024))

;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 3 1024 1024))

;; Set all default dirs
(require 'litter)

(setq-default
 ;; Make buffer names unique
 uniquify-buffer-name-style 'post-forward
 ;; Don't create lock files
 create-lockfiles nil
 ;; Calendar
 calendar-latitude 46.7667
 calendar-longitude 23.5833
 ;; Consider all themes safe
 custom-safe-themes t
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
 whitespace-line-column 180
 ;; set language for time to Eng
 system-time-locale "C"
 ;; Always save desktop
 desktop-save t
 ;; Move to help buffer when opened
 help-window-select t)

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
(global-unset-key (kbd "C-S-K"))
(global-set-key (kbd "C-S-K") 'kill-whole-line)

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

;; Window navigation
(global-set-key (kbd "C-S-o") 'previous-window-any-frame)

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

;; Show beautified page breaks
(straight-use-package 'page-break-lines)
(global-page-break-lines-mode t)
;; Revert buffer when the file changes on disk
(global-auto-revert-mode 1)
;; Delete selected text when starting to type
(delete-selection-mode 1)
;; Save state between sessions
(desktop-save-mode t)

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
(require 'exec-path)
(require 'litter)
(require 'othermodes-settings)
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

;; Programming
(require 'other-languages-config)
(require 'javascript-config)
(require 'json-config)
(require 'xml-config)
(require 'web-config)
(require 'sh-config)
(require 'elisp-config)
(require 'typescript-config)

(require 'blog-settings)

;; Load Abbrevs
;; (require 'abbrevs)

(provide 'general-settings)
;;; general-settings ends here

