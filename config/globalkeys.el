;;; globalkeys.el --- Global keys

;;; Commentary:
;; Only global keys

;;; Code:
(global-set-key (kbd "C-S-s") 'query-replace)
(global-set-key (kbd "C-x C-s") 'my-save-buffer)
(global-set-key (kbd "C-S-d") 'delete-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x m") 'magit-status)
(global-set-key (kbd "C-`") 'er/expand-region)
(global-set-key (kbd "<C-tab>") 'indent-relative)
(global-set-key (kbd "C-c C-f") 'hs-toggle-hiding)
(global-set-key (kbd "C-h C-s") 'elisp-index-search)
(global-set-key (kbd "C-o") 'company-complete)
(global-set-key (kbd "C-x K") 'kill-other-buffer)

;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-S-f") 'helm-projectile)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-.") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-/") 'helm-find)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
(global-set-key (kbd "C-x C-S-f") 'helm-projectile)
(global-set-key (kbd "C-x g") 'helm-do-grep-recursive)
(global-set-key (kbd "C-x C-b") 'helm-resume)


;; Smart-compile
(global-set-key (kbd "<f9>") 'smart-compile)

;; Change to Message buffer
(global-set-key (kbd "<f1>")
                '(lambda () (interactive) (switch-to-buffer "*Messages*")))

;; Widow resizing
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Smartparens
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
(define-key sp-keymap (kbd "C-M-e") 'sp-end-of-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-y") 'sp-backward-unwrap-sexp)
(define-key sp-keymap (kbd "C-M-o") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "C-}") 'sp-select-next-thing)
(define-key sp-keymap (kbd "C-{") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
(define-key sp-keymap (kbd "<C-M-backspace>") 'sp-backward-kill-sexp)
(define-key sp-keymap (kbd "C-M-0") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-9") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-8") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-7") 'sp-backward-barf-sexp)
(define-key sp-keymap (kbd "C-M-y") 'sp-backward-unwrap-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)
(define-key sp-keymap (kbd "C-(") 'my-wrap-with-round-paren)
(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-:") 'mc/mark-pop)

(global-set-key (kbd "C-\\") 'delete-to-previous-line)
(global-set-key (kbd "C-S-y") 'yank-next)

;; Export
(provide 'globalkeys)

;;; globalkeys.el ends here
