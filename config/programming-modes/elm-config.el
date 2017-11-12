;;; elm-config.el --- Elm config

;;; Commentary:
;; My Elm config file

;;; Code:
(with-eval-after-load 'elm-mode
  (progn
    (message "elm-mode loaded")

    (setq-default elm-format-on-save t)))

(defun start-elm-flycheck ()
  "Start flycheck and setup it for elm."
  (flycheck-mode)
  (flycheck-elm-setup))


(add-hook 'elm-mode-hook 'start-elm-flycheck)
(add-hook 'elm-mode-hook 'linum-mode)

(provide 'elm-config)
;;; elm-config.el ends here
