;;; web-config --- Web config

;;; Commentary:
;; My config for HTML & CSS (+ preprocessors)

;;; Code:
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

(with-eval-after-load 'web-mode
  (progn
    (message "web-mode loaded")

    (setq web-mode-markup-indent-offset 2)

    ;; Keys
    (define-key web-mode-map (kbd "C-,") 'sgml-tag)
    (define-key web-mode-map (kbd "C-o") 'open-file-in-browser)
    (define-key web-mode-map (kbd "C-j") 'newline-and-indent)
    (define-key web-mode-map (kbd "C-;") 'emmet-expand-line)
    (define-key web-mode-map (kbd "M-n") 'sp-html-next-tag)
    (define-key web-mode-map (kbd "M-p") 'sp-html-previous-tag)
    (define-key web-mode-map (kbd "C-M-n") 'web-mode-element-next)
    (define-key web-mode-map (kbd "C-M-p") 'web-mode-element-previous)
    (define-key web-mode-map (kbd "C-M-d") 'web-mode-element-child)
    (define-key web-mode-map (kbd "C-M-u") 'web-mode-element-parent)
    (define-key web-mode-map (kbd "C-M-a") 'web-mode-element-beginning)
    (define-key web-mode-map (kbd "C-M-e") 'web-mode-element-end)
    (define-key web-mode-map (kbd "C-M-f") 'web-mode-attribute-next)
    (define-key web-mode-map (kbd "C-M-b") 'web-mode-attribute-previous)
    (define-key web-mode-map (kbd "C-M-k") 'web-mode-element-kill)
    (define-key web-mode-map (kbd "C-S-i") 'web-mode-element-content-select)
    (define-key web-mode-map (kbd "C-S-c") 'web-mode-element-clone)
    (define-key web-mode-map (kbd "<C-M-backspace>") 'backward-kill-element)))

(with-eval-after-load 'css-mode
  (progn
    (message "css-mode loaded")

    (setq css-indent-offset 2)))

(add-hook 'css-mode-hook 'rainbow-mode)
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

;; Hooks
(add-hook 'web-mode-hook 'flycheck-mode)
(add-hook 'web-mode-hook 'skewer-html-mode)
(add-hook 'web-mode-hook 'linum-mode)
(add-hook 'web-mode-hook 'hs-minor-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'linum-mode)
(add-hook 'css-mode-hook 'flycheck-mode)
(add-hook 'css-mode-hook 'hs-minor-mode)

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


(provide 'web-config)
;;; html-config.el ends here
