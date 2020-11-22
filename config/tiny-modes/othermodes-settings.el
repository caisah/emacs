;;; othermodes-settings.el --- Enabe other modes

;;; Commentary:
;; Settings for other modes

;;; Code:

;; Re-Builder
(setq-default reb-re-syntax 'string)


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


;; Yasnippet https://github.com/capitaomorte/yasnippet
(setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; KeyFreq https://github.com/dacap/keyfreq
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)


;;  JSON mode
(defun my-json-hook ()
  "Personal hook for json-mode."
  (progn
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2)

    (local-set-key (kbd "C-c C-b") 'json-mode-beautify)
    (local-set-key (kbd "C-c C-f") 'hs-toggle-hiding)))

(add-hook 'json-mode-hook 'flycheck-mode)
(add-hook 'json-mode-hook 'hs-minor-mode)
(add-hook 'json-mode-hook 'my-json-hook)

;; XML
(add-hook 'nxml-mode-hook 'sgml-mode)
(add-hook 'nxml-mode-hook 'hs-minor-mode)

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
(require 'editorconfig)

;; Enable emoji
(global-emojify-mode 1)

;; Use package lint for elisp
(eval-after-load 'flycheck
  '(flycheck-package-setup))

(provide 'othermodes-settings)
;;; othermodes-settings.el ends here
