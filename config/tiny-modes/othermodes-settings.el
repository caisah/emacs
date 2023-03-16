;;; othermodes-settings.el --- Enabe other modes

;;; Commentary:
;; Settings for other modes

;;; Code:

;; Re-Builder
(setq-default reb-re-syntax 'string)

;; Use markdown-mode package
(straight-use-package 'markdown-mode)
;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode 'pandoc-mode)

;; Hide/show
(defface hs-face nil
         "Basic face for hide/show."
         :group 'basic-faces)

(setq-default hs-set-up-overlay
       (defun my-display-code-line-counts (ov)
         (when (eq 'code (overlay-get ov 'hs))
           (overlay-put ov 'display
                        (propertize
                         (format " ... %d lines ... "
                                 (count-lines (overlay-start ov)
                                              (overlay-end ov)))
                         'face 'hs-face)))))

;; Use yas
(straight-use-package 'yasnippet)
;; Enable yasnippet globally
(yas-global-mode)

;; Enable eldoc globally
(global-eldoc-mode t)

;; YAML
(straight-use-package 'yaml-mode)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; KeyFreq https://github.com/dacap/keyfreq
(straight-use-package 'keyfreq)
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"
               "<!--"
               sgml-skip-tag-forward
               nil))

;; nov-mode for epub
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;; dot-env-mode for .env files
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))

;; Editor config
(straight-use-package 'editorconfig)
(require 'editorconfig)

;; Use prettier-js package
(straight-use-package 'prettier-js)
;; Use undo-tree package
(straight-use-package 'undo-tree)
;; Enable undo tree mode
(global-undo-tree-mode 1)

(straight-use-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(straight-use-package 'pretty-symbols)

(straight-use-package 'rainbow-delimiters)

(straight-use-package 'expand-region)

(straight-use-package 'rainbow-mode)

(straight-use-package 'ag)

;; Show beautified page breaks
(straight-use-package 'page-break-lines)
(global-page-break-lines-mode t)

(provide 'othermodes-settings)
;;; othermodes-settings.el ends here
