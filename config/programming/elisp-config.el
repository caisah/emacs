;;; elisp-config.el --- emacs lisp programming

;;; Commentary:
;; Config for .el (emacs lisp) files

;;; Code:
;; Abbrevs:
(define-abbrev-table 'emacs-lisp-mode-abbrev-table '(
  ("lam" "lambda")
  ("def" "defun")
  ("mes" "message")
  ("int" "(interactive)")
  ("sav" "(save-excursion)")
))

;; Hooks
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook 'pretty-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'abbrev-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)

(provide 'elisp-config)
;;; elisp-config.el ends here
