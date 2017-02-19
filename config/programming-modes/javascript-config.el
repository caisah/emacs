;;; javascript-config.el --- JavaScript config

;;; Commentary:
;; My JavaScript config file

;;; Notes to self:
;;; add hippie expand `hippie-expand-try-functions-list

;;; Code:

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(defun myjs-before-hooks ()
  "Run before all hooks:
   - change mode name to JS2;"

  (message "Running 'before' hooks for js2-mode in buffer.")

  (setq mode-name "JS2"))


(defun myjs-after-hooks ()
  "Run after all hooks are executed."
  (message "Running 'after' hooks for js2-mode in buffer")

  ;; set backends for company
  (set (make-local-variable 'company-backends)
       '(company-dabbrev-code
         company-dabbrev
         company-tern
         company-files
         company-keywords)))


;; order matters
(add-hook 'js2-mode-hook 'myjs-after-hooks)
(add-hook 'js2-mode-hook 'whitespace-mode)
(add-hook 'js2-mode-hook 'company-mode)
(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'js2-mode-hook 'smartparens-strict-mode)
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook 'hs-minor-mode)
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'linum-mode)
(add-hook 'js2-mode-hook 'myjs-before-hooks)


(with-eval-after-load 'js2-mode
  (progn
    (message "js2-mode loaded")

    (setq-default
     js2-mode-show-parse-errors nil
     js2-mode-show-strict-warnings nil
     js2-strict-var-redeclaration-warning nil
     js2-strict-missing-semi-warning nil
     js2-mode-show-strict-warnings nil
     js2-include-browser-externs t
     js2-include-node-externs t
     js2-basic-offset 2)))


;; Export
(provide 'javascript-config)
;;; javascript-config.el ends here
