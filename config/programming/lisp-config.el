;;; lisp-config.el --- common lisp programming

;;; Commentary:
;; Config for common lisp

;;; Code:
(require 'slime)

(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))

(add-hook 'lisp-mode-hook 'slime-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'lisp-mode-hook 'pretty-symbols-mode)
(add-hook 'lisp-mode-hook 'abbrev-mode)
(add-hook 'lisp-mode-hook 'linum-mode)
(add-hook 'lisp-mode-hook 'flycheck-mode)
(add-hook 'lisp-mode-hook 'whitespace-mode)


(provide 'lisp-config)
;;; lisp-config.el ends here
