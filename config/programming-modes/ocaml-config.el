;;; ocaml-config.el --- OCaml config

;;; Commentary:
;; My OCaml config

;;; Code:
(with-eval-after-load 'merlin
  ;; Disable Merlin's own error checking
  (setq-default merlin-error-after-save nil)

  ;; Add ocaml-merlin to flycheck checkers
  (flycheck-ocaml-setup))

(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'tuareg-mode-hook 'linum-mode)
(add-hook 'tuareg-mode-hook 'flycheck-mode)


(provide 'ocaml-config)
;;; ocaml-config.el ends here
