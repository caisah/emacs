;;; json.el --- JSON

;;; Commentary:
;; Settings for json

;;; Code:


(straight-use-package 'json-mode)
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

(provide 'json)
