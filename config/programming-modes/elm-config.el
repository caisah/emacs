;;; elm-config.el --- Elm config

;;; Commentary:
;; My Elm config file

;;; Code:
(with-eval-after-load 'elm-mode
  (progn
    (message "elm-mode loaded")))

(add-hook 'elm-mode-hook 'flycheck-mode)
(add-hook 'elm-mode-hook 'linum-mode)

(provide 'elm-config)
;;; elm-config.el ends here
