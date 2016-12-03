;;; typescript-config.el --- Typescript config

;;; Commentary:
;; My TypeScript config file

;;; Code:
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(defun ts-keys ()
  "Enable local keys for typescript-mode."
  (define-key typescript-mode-map (kbd "C-c C-f") 'hs-toggle-hiding)
  (define-key typescript-mode-map (kbd "C-c C-e") 'shell-execute-last-command)
  (define-key typescript-mode-map (kbd "M-,") 'tss-popup-help)
  (define-key typescript-mode-map (kbd "M-.") 'tss-jump-to-definition)
  (define-key typescript-mode-map (kbd "C-c i") 'tss-implement-definition))

(defun change-name-&-disable-autocomplete ()
  "Change name to TypeScript & disable autocomplete mode."
  (setq mode-name "TypeScript")
  (auto-complete-mode 0))

;; Typescript tools
(require 'tss)
(tss-config-default)

;; Do setting recommended configuration
(add-hook 'typescript-mode-hook 'change-name-&-disable-autocomplete)
(add-hook 'typescript-mode-hook 'pretty-symbols-mode)
(add-hook 'typescript-mode-hook 'linum-mode)
(add-hook 'typescript-mode-hook 'abbrev-mode)
(add-hook 'typescript-mode-hook 'smartparens-strict-mode)
(add-hook 'typescript-mode-hook 'hs-minor-mode)
(add-hook 'typescript-mode-hook 'flycheck-mode)
(add-hook 'typescript-mode-hook 'whitespace-mode)
(add-hook 'typescript-mode-hook 'ts-keys)
(add-hook 'typescript-mode-hook 'tss-setup-current-buffer)


;; Export
(provide 'typescript-config)

;;; typescript-config.el ends here
