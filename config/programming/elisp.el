;; Abbrevs:
(define-abbrev-table 'emacs-lisp-mode-abbrev-table '(
  ("lam" "lambda")						     
  ("def" "defun")
  ("mes" "message")
  ("int" "(interactive)")
  ("sav" "(save-excursion)")
))


;; Enable rainbow delimiters
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook 'pretty-symbols-mode) 
(add-hook 'emacs-lisp-mode-hook 'abbrev-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)


(provide 'elisp-config)
