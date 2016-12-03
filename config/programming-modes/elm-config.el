;;; elm-config.el --- Elm config

;;; Commentary:
;; My Elm config file

(require 'elm-mode)

;;; Code:
(add-hook 'elm-mode-hook 'flycheck-mode)
(add-hook 'elm-mode-hook 'linum-mode)

(provide 'elm-config)
;;; elm-config.el ends here
