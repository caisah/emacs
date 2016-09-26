;;; ocaml-config.el --- OCaml config

;;; Commentary:
;; My OCaml config

;;; Code:
(with-eval-after-load 'merlin
  ;; Disable Merlin's own error checking
  (setq merlin-error-after-save nil)

  ;; Enable Flycheck checker
  (flycheck-ocaml-setup))

(add-hook 'tuareg-mode-hook #'merlin-mode)


(provide 'ocaml-config)
;;; ocaml-config.el ends here
