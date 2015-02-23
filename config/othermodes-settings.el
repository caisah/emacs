;;; othermodes-settings.el --- Enabe other modes

;;; Commentary:
;; Settings for other modes

;;; Code:

;; Desktop
(desktop-save-mode 1)
(setq-default desktop-dirname "~/.emacs.d/cache")
(setq-default desktop-path (list desktop-dirname))
(setq-default desktop-base-file-name "emacs-desktop")
(setq-default indent-tabs-mode nil)
(setq-default desktop-save t)

;; Re-Builder
(setq-default reb-re-syntax 'string)

;; Emmet https://github.com/smihica/emmet-mode
(setq-default emmet-preview-default nil)

;; Org Mode
;; enable toc
(setq-default org-src-fontify-natively t)
(eval-after-load "org-toc-autoloads"
  '(progn
     (if (require 'org-toc nil t)
         (add-hook 'org-mode-hook 'org-toc-enable)
       (warn "org-toc not found"))))
(add-hook 'org-mode-hook 'flyspell-mode)

;; Calendar
(setq-default calendar-latitude 46.7667)
(setq-default calendar-longitude 23.5833)

;; eww
(setq-default eww-search-prefix "https://google.com/search?q=")
(setq-default shr-color-visible-luminance-min 70)

;; Httpd https://github.com/skeeto/emacs-web-server
(require 'simple-httpd)
(setq httpd-root "~/Documents/server/")

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

;; Page Break Lines https://github.com/purcell/page-break-lines
(require 'page-break-lines)
(global-page-break-lines-mode t)

;; Multiple Cursors https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)

;; Kill Ring
(require 'browse-kill-ring)

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


;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; Projectile
(projectile-global-mode)
(setq-default projectile-enable-caching t)


;; SmartParens:
(require 'smartparens-config)
(require 'smartparens-html)

(smartparens-global-mode t)
(smartparens-strict-mode)
(setq sp-highlight-pair-overlay nil)

;; Wrap the next expression in a pair of parens
(defun my-wrap-with-round-paren (&optional arg)
  "Wrap ARG with in round parens ()."
  (interactive "p")
  (sp-select-next-thing-exchange arg)
  (execute-kbd-macro (kbd "(")))
(setq-default sp-hybrid-kill-excessive-whitespace t)

;; Smart Mode Line https://github.com/Bruce-Connor/smart-mode-line
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'dark)
(setq sml/mode-width 15)
(setq-default rm-excluded-modes
              (list " Anzu" " ARev" " SP/s" " SP" " Abbrev" " Isearch"
                    " A" " Guide"  " Undo-Tree" " PgLn" " MRev"
                    " skewer-html" " skewer-css"" Emmet" " hs"
                    " λ" " Rbow" " vl" " Wrap" " Helm" " Projectile" " yas"
                    " company" " Tern" " ws" " WS"))

;; Tramp http://www.emacswiki.org/TrampMode
(require 'tramp)
(setq tramp-default-method "ssh")

;; Yasnippet https://github.com/capitaomorte/yasnippet
(setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook 'linum-mode)

;; KeyFreq https://github.com/dacap/keyfreq
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)


;; Lint on JSON
(add-hook 'json-mode-hook 'flycheck-mode)

;; Export
(provide 'othermodes-settings)

;;; othermodes-settings.el ends here
