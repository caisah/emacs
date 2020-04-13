;;; smartparens-settings.el -- Smart parens
;;; Commentary:
;;  Settings for parens


;;; Code:
(with-eval-after-load 'smartparens
  (progn
    (message "My init :: smartparens loaded")

    (require 'smartparens-config)
    (require 'smartparens-html)

    (setq-default sp-highlight-pair-overlay nil
                  sp-hybrid-kill-excessive-whitespace t)
    ;; Keys
    (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
    (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

    (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
    (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key smartparens-mode-map (kbd "C-M-a") 'sp-beginning-of-sexp)
    (define-key smartparens-mode-map (kbd "C-M-e") 'sp-end-of-sexp)

    (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
    (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

    (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
    (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

    (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
    (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

    (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
    (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

    (define-key smartparens-mode-map (kbd "C-M-8") 'sp-forward-slurp-sexp)
    (define-key smartparens-mode-map (kbd "C-M-7") 'sp-forward-barf-sexp)

    (define-key smartparens-mode-map (kbd "C-(") 'my-wrap-with-round-paren)

    (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
    (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
    (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

    ;; let C-S-d bound to 'delete-region
    (define-key smartparens-strict-mode-map [remap delete-region] nil)))

(smartparens-global-mode t)

;; Wrap the next expression in a pair of parens
(defun my-wrap-with-round-paren (&optional arg)
  "Wrap ARG with in round parens ()."
  (interactive "p")
  (sp-select-next-thing-exchange arg)
  (execute-kbd-macro (kbd "(")))

;; export
(provide 'smartparens-settings)
;;; smartparens-settings ends here
