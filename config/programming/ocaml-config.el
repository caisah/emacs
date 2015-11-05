;;; ocaml-config.el --- Ocaml config

;;; Commentary:
;; My OCaml config
;;; Code:

;; Merlin
(defun disable-ac ()
  "Disable auto-complete mode."
  (auto-complete-mode 0))

(with-eval-after-load 'merlin
  ;; Disable Merlin's own error checking
  (setq merlin-error-after-save nil)

  ;; Enable Flycheck checker
  (flycheck-ocaml-setup))

;; directory containing merlin.el
(autoload 'merlin-mode "merlin" "Merlin mode" t)

(add-hook 'merlin-mode-hook 'disable-ac)

;; Utop
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)

;; Keys
(defun ocaml-keys ()
  "Enable local keys for tuareg-mode."
  (define-key tuareg-mode-map (kbd "C-c C-z") 'utop))

(add-hook 'tuareg-mode-hook 'utop-minor-mode)
(add-hook 'tuareg-mode-hook 'linum-mode)
(add-hook 'tuareg-mode-hook 'smartparens-strict-mode)
(add-hook 'tuareg-mode-hook 'hs-minor-mode)
(add-hook 'tuareg-mode-hook 'whitespace-mode)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'tuareg-mode-hook 'flycheck-mode)
(add-hook 'tuareg-mode-hook 'ocaml-keys)

;; Exports
(provide 'ocaml-config)
;;; ocaml-config.el ends here
