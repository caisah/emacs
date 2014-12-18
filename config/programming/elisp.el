;; Abbrevs:
(define-abbrev-table 'emacs-lisp-mode-abbrev-table '(
  ("lam" "lambda")						     
  ("def" "defun")
  ("mes" "message")
  ("int" "(interactive)")
  ("sav" "(save-excursion)")
))

(defun local-elisp-keys ()
  (bind-smartparens-locally))

;; Enable rainbow delimiters
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook 'pretty-symbols-mode) 
(add-hook 'emacs-lisp-mode-hook 'abbrev-mode)
(add-hook 'emacs-lisp-mode-hook 'local-elisp-keys)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)


(provide 'elisp-config)
