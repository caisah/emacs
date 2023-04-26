;;; elisp-config.el --- emacs lisp programming

;;; Commentary:
;; Config for .el (Emacs Lisp) files

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
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook 'pretty-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'abbrev-mode)
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)

(straight-use-package 'elisp-format)
(straight-use-package 'elisp-slime-nav)

(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(provide 'elisp-config)
;;; elisp-config.el ends here
