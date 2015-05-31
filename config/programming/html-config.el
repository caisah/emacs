;;; html-config --- Web config

;;; Commentary:
;; My config for HTML & CSS (+ preprocessors)

;;; Code:
(require 'web-mode)

(setq web-mode-markup-indent-offset 4)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jst\\'" . web-mode))

;; Functions
(defun open-file-in-browser()
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url filename)))

(defun backward-kill-element ()
  "Backward kill web element."
  (interactive)
  (backward-word)
  (web-mode-element-kill))

;; Keys for html
(defun html-keys ()
  "Keys used in HTML."
  (local-set-key (kbd "C-,") 'sgml-tag)
  (local-set-key (kbd "C-o") 'open-file-in-browser)
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (local-set-key (kbd "C-j") 'newline-and-indent)
  (local-set-key (kbd "C-;") 'emmet-expand-line)
  (local-set-key (kbd "M-n") 'sp-html-next-tag)
  (local-set-key (kbd "M-p") 'sp-html-previous-tag)
  (local-set-key (kbd "C-M-n") 'web-mode-element-next)
  (local-set-key (kbd "C-M-p") 'web-mode-element-previous)
  (local-set-key (kbd "C-M-d") 'web-mode-element-child)
  (local-set-key (kbd "C-M-u") 'web-mode-element-parent)
  (local-set-key (kbd "C-M-a") 'web-mode-element-beginning)
  (local-set-key (kbd "C-M-e") 'web-mode-element-end)
  (local-set-key (kbd "C-M-f") 'web-mode-attribute-next)
  (local-set-key (kbd "C-M-b") 'web-mode-attribute-previous)
  (local-set-key (kbd "C-M-k") 'web-mode-element-kill)
  (local-set-key (kbd "C-S-i") 'web-mode-element-content-select)
  (local-set-key (kbd "C-S-c") 'web-mode-element-clone)
  (local-set-key (kbd "<C-M-backspace>") 'backward-kill-element))


(defun enable-ss-keys ()
  "Keys for CSS preprocessors."
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (local-set-key (kbd "C-j") 'newline-and-indent))

;; Hooks
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'flycheck-mode)
(add-hook 'web-mode-hook 'html-keys)
(add-hook 'web-mode-hook 'skewer-html-mode)
(add-hook 'web-mode-hook 'linum-mode)
(add-hook 'web-mode-hook 'skewer-css-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'linum-mode)

;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; LESS
(add-hook 'less-css-mode-hook 'linum-mode)
(add-hook 'less-css-mode-hook 'flycheck-mode)
(add-hook 'less-css-mode-hook 'enable-ss-keys)
(add-hook 'less-css-mode-hook 'hs-minor-mode)

;; Sass
(add-hook 'scss-mode-hook 'linum-mode)
(add-hook 'scss-mode-hook 'flycheck-mode)
(add-hook 'scss-mode-hook 'enable-ss-keys)
(add-hook 'scss-mode-hook 'hs-minor-mode)


(provide 'html-config)
;;; html-config.el ends here
