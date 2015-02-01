;;; javascript-config.el --- JavaScript config

;;; Commentary:
;; My JavaScript config file

;;; Code:
(setq-default js2-show-parse-errors nil
              js2-strict-missing-semi-warning nil
              js2-strict-trailing-comma-warning t
              js2-basic-offset 4
              js2-global-externs '("module" "require" "exports" "sinon" "assert"
                                   "refute" "setTimeout" "clearTimeout" "setInterval"
                                   "clearInterval" "location" "__dirname" "console"
                                   "JSON" "$" "jQuery" "doT" "_" "Backbone"
                                   "skewer"))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'pretty-symbol-patterns '(?ƒ lambda "\\<function\\>" (js2-mode)))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
;; Abbrevs
(define-abbrev-table 'js2-mode-abbrev-table '(("ret" "return")))

(defun skewer-eval-region (beg end)
  "Execute the region from BEG to END as JavaScript code in the attached browsers."
  (interactive "r")
  (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))

;; Keys for js2-mode
(defun js-keys ()
  "Enable local keys for js2-mode."
  (define-key js2-mode-map (kbd "C-c C-f") 'hs-toggle-hiding)
  (define-key js2-mode-map (kbd "C-c C-e") 'shell-execute-last-command))


;; Keys for skewer
(defun skewer-keys ()
  "Enable local keys for skewer-mode."
  (local-set-key (kbd "C-x C-r") 'skewer-eval-region)
  (local-set-key (kbd "C-M-k") 'sp-kill-hybrid-sexp)
  (local-set-key (kbd "C-k") 'kill-line))

;; Keys for js-comint/Node.js
(defun node-keys ()
  "Enable local keys for nodejs."
  (interactive)
  ;; disable skewer-mode
  (progn
    (if (assoc 'skewer-mode minor-mode-alist)
        (setq skewer-mode nil))
    (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
    (local-set-key (kbd "C-x C-r") 'js-send-region)
    (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
    (local-set-key (kbd "C-x l") 'js-send-buffer)
    (local-set-key (kbd "C-x L") 'js-send-buffer-and-go)))

;; JS comint
(require 'js-comint)
(setq inferior-js-program-command "nodejs")
(setenv "NODE_NO_READLINE" "1")
(setq inferior-js-mode-hook
      (lambda ()
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\033\\[[0-9]+[GKJ]" "" output)))))
(defalias 'nodejs 'run-js)

;; Hooks
(add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))

(add-hook 'js2-mode-hook 'pretty-symbols-mode)
(add-hook 'js2-mode-hook 'linum-mode)
(add-hook 'js2-mode-hook 'abbrev-mode)
(add-hook 'js2-mode-hook 'smartparens-strict-mode)
(add-hook 'js2-mode-hook 'hs-minor-mode)
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'yas-minor-mode)
(add-hook 'js2-mode-hook 'enable-jshint)
(add-hook 'js2-mode-hook 'js-keys)
(add-hook 'js2-mode-hook 'whitespace-mode)

(add-hook 'skewer-mode-hook 'skewer-keys)

;; Typescript
(require 'typescript-mode)

(add-hook 'typescript-mode-hook (lambda () (setq mode-name "TypeScript")))

(add-hook 'typescript-mode-hook 'pretty-symbols-mode)
(add-hook 'typescript-mode-hook 'linum-mode)
(add-hook 'typescript-mode-hook 'abbrev-mode)
(add-hook 'typescript-mode-hook 'smartparens-strict-mode)
(add-hook 'typescript-mode-hook 'hs-minor-mode)
(add-hook 'typescript-mode-hook 'flycheck-mode)
(add-hook 'typescript-mode-hook 'yas-minor-mode)
(add-hook 'typescript-mode-hook 'whitespace-mode)


;; Export
(provide 'javascript-config)

;;; javascript-config.el ends here
