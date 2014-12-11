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

;; Kesys for js2 mode
(defun js-keys ()
  "Keys used in Javascript"
  (interactive)
  (local-set-key (kbd "C-x C-r") 'skewer-eval-region)
  (local-set-key (kbd "C-c C-f") 'hs-toggle-hiding)
  (local-set-key (kbd "C-M-k") 'sp-kill-hybrid-sexp)
  (local-set-key (kbd "C-k") 'kill-line)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))


(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'js2-mode-hook 'pretty-symbols-mode)
(add-hook 'js2-mode-hook 'linum-mode)
(add-hook 'js2-mode-hook 'abbrev-mode)
(add-hook 'js2-mode-hook 'js-keys)
(add-hook 'js2-mode-hook 'smartparens-strict-mode) 
(add-hook 'js2-mode-hook 'hs-minor-mode)
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'yas-minor-mode)
(add-hook 'js2-mode-hook 'company-mode)
(add-hook 'js2-mode-hook 'enable-jshint)

(provide 'js-config)
