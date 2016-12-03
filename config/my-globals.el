;;; my-globals.el --- Global settings & modes

;;; Commentary:
;; Global settings & modes

;;; Code:


;; ;; Smartparens
;; (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
;; (define-key sp-keymap (kbd "C-M-e") 'sp-end-of-sexp)
;; (define-key sp-keymap (kbd "C-M-a") 'sp-beginning-of-sexp)
;; (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
;; (define-key sp-keymap (kbd "C-M-y") 'sp-backward-unwrap-sexp)
;; (define-key sp-keymap (kbd "C-M-o") 'sp-unwrap-sexp)
;; (define-key sp-keymap (kbd "C-}") 'sp-select-next-thing)
;; (define-key sp-keymap (kbd "C-{") 'sp-select-previous-thing)
;; (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
;; (define-key sp-keymap (kbd "<C-M-backspace>") 'sp-backward-kill-sexp)
;; (define-key sp-keymap (kbd "C-M-0") 'sp-forward-slurp-sexp)
;; (define-key sp-keymap (kbd "C-M-9") 'sp-backward-slurp-sexp)
;; (define-key sp-keymap (kbd "C-M-8") 'sp-forward-barf-sexp)
;; (define-key sp-keymap (kbd "C-M-7") 'sp-backward-barf-sexp)
;; (define-key sp-keymap (kbd "C-M-y") 'sp-backward-unwrap-sexp)
;; (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)
;; (define-key sp-keymap (kbd "C-(") 'my-wrap-with-round-paren)
;; (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)

;; ;; Multiple cursors
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; (global-set-key (kbd "C-:") 'mc/mark-pop)


;; (global-set-key (kbd "C-S-y") 'yank-next)

;; ;; ORG
;; (add-hook 'org-mode-hook
;;           '(lambda ()
;; (define-key org-mode-map (kbd "C-M-p") 'outline-previous-visible-heading)
;; (define-key org-mode-map (kbd "C-M-n") 'outline-next-visible-heading)
;; (define-key org-mode-map (kbd "C-M-u") 'outline-up-

;; ;; Company

;; ;; Other modes
;; (with-eval-after-load 'nxml-mode
;;   (define-key nxml-mode-map (kbd "C-c C-f") 'hs-toggle-hiding))

;; Export
(provide 'my-globals)

;;; my-globals.el ends here
