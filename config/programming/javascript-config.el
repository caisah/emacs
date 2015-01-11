;; Javascript

(setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "$" "jQuery" "doT" "_" "Backbone" "skewer"))

                                        ; Let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t)
(setq-default js2-basic-offset 4)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'pretty-symbol-patterns '(?ƒ lambda "\\<function\\>" (js2-mode)))

(define-abbrev-table 'js2-mode-abbrev-table '(
                                              ("ret" "return")))

(defun skewer-eval-region (beg end)
  "Execute the region as JavaScript code in the attached browsers."
  (interactive "r")
  (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))

;; Keys for js2-mode
(defun js-keys ()
  (define-key js2-mode-map (kbd "C-c C-f") 'hs-toggle-hiding))


;; Keys for skewer
(require 'skewer-mode)
(define-key skewer-mode-map (kbd "C-x C-r") 'skewer-eval-region)
(define-key skewer-mode-map (kbd "C-M-k") 'sp-kill-hybrid-sexp)
(define-key skewer-mode-map (kbd "C-k") 'kill-line)

;; Keys for js-comint/Node.js
(defun node-keys ()
  (interactive)
  (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
  (local-set-key (kbd "C-x C-r") 'js-send-region)
  (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
  (local-set-key (kbd "C-c b") 'js-send-buffer)
  (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go))

;; JS comint
(require 'js-comint)
(setq inferior-js-program-command "nodejs")
(setenv "NODE_NO_READLINE" "1")
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


(provide 'javascript-config)
