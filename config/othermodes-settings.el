;; Httpd https://github.com/skeeto/emacs-web-server
(require 'simple-httpd)
(setq httpd-root "~/Documents/javascript/")

;; Smart Compile http://www.emacswiki.org/emacs/SmartCompile
(require 'smart-compile)

;; Undo Tree http://www.emacswiki.org/UndoTree
(require 'undo-tree)
(global-undo-tree-mode)

;; Expand Region https://github.com/magnars/expand-region.el
(require 'expand-region)

;; Anzu https://github.com/syohex/emacs-anzu
(require 'anzu)
(global-anzu-mode t)

;; Rainbow Delimiters https://github.com/jlr/rainbow-delimiters
(require 'rainbow-delimiters)

;; Re-Builder
(require 're-builder)
(setq reb-re-syntax 'string)

;; Page Break Lines https://github.com/purcell/page-break-lines
(require 'page-break-lines)
(global-page-break-lines-mode t)

;; Emmet https://github.com/smihica/emmet-mode
(require 'emmet-mode)
(setq emmet-preview-default nil) 

;; Multiple Cursors https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)

;; Org Mode
(require 'org)

;; Nginx mode https://github.com/ajc/nginx-mode
(require 'nginx-mode)

;; Calendar
(setq calendar-latitude 46.7667)
(setq calendar-longitude 23.5833)

;; eww
(setq eww-search-prefix "https://google.com/search?q=")

;; Company
(setq company-tern-meta-as-single-line t)
(setq company-tooltip-align-annotations t)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-n") #'company-select-previous))

;; Ibuffer
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

;; Kill Ring
(require 'browse-kill-ring)

;; Markdown mode 
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; org mode
(add-hook 'org-mode-hook 'flyspell-mode)

;; Projectile
(projectile-global-mode)
(setq projectile-enable-caching t)

;; SmartParens:
(require 'smartparens-config)
(require 'smartparens-html)

(smartparens-global-mode t)
(smartparens-strict-mode)
(setq sp-highlight-pair-overlay nil)

;; Wrap the next expression in a pair of parens
(defun my-wrap-with-round-paren (&optional arg)
  (interactive "p")
  (sp-select-next-thing-exchange arg)
  (execute-kbd-macro (kbd "(")))


;; Smart Mode Line https://github.com/Bruce-Connor/smart-mode-line
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'dark)
(setq sml/mode-width 15)
(setq rm-excluded-modes
      (list " Anzu" " ARev" " SP/s" " SP" " Abbrev" " Isearch"
            " A" " Guide"  " Undo-Tree" " PgLn" " MRev"
            " skewer" " skewer-html" " skewer-css"" Emmet" " hs"
            " λ" " Rbow" " vl" " Wrap" " Helm" " Projectile" " yas"
            " company" " Tern"
            ))

;; Tramp http://www.emacswiki.org/TrampMode
(require 'tramp)
(setq tramp-default-method "ssh")

;; Yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-reload-all)


(provide 'othermodes-settings)
