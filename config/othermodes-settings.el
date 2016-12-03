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


;; SmartParens:
(require 'smartparens-config)
(require 'smartparens-html)

(smartparens-global-mode t)
(smartparens-strict-mode)
(setq sp-highlight-pair-overlay nil)

;; Wrap the next expression in a pair of parens
(defun my-wrap-with-round-paren (&optional arg)
  "Wrap ARG with in round parens ()."
  (interactive "p")
  (sp-select-next-thing-exchange arg)
  (execute-kbd-macro (kbd "(")))
(setq-default sp-hybrid-kill-excessive-whitespace t)


;; Hide/show
(defface hs-face nil
         "Basic face for hide/show."
         :group 'basic-faces)

(setq hs-set-up-overlay
       (defun my-display-code-line-counts (ov)
         (when (eq 'code (overlay-get ov 'hs))
           (overlay-put ov 'display
                        (propertize
                         (format " ... %d lines ... "
                                 (count-lines (overlay-start ov)
                                              (overlay-end ov)))
                         'face 'hs-face)))))


;; Tramp http://www.emacswiki.org/TrampMode
(require 'tramp)
(setq tramp-default-method "ssh")

;; Yasnippet https://github.com/capitaomorte/yasnippet
(setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook 'linum-mode)

;; KeyFreq https://github.com/dacap/keyfreq
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)


;;  JSON mode
(defun set-json-mode-keys ()
  "Set keys for json-mode."
  (local-set-key (kbd "C-c C-b") 'json-mode-beautify)
  (local-set-key (kbd "C-c C-f") 'hs-toggle-hiding))

(defun set-json-indentation ()
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2))

(add-hook 'json-mode-hook 'flycheck-mode)
(add-hook 'json-mode-hook 'hs-minor-mode)
(add-hook 'json-mode-hook 'set-json-mode-keys)
(add-hook 'json-mode-hook 'set-json-indentation)

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


(provide 'othermodes-settings)
;;; othermodes-settings.el ends here
