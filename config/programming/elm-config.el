;;; elm-config.el --- Elm config

;;; Commentary:
;; My Elm config file

;;; Code:
(require 'elm-mode)

(add-hook 'elm-mode-hook 'flycheck-mode)
(add-hook 'elm-mode-hook 'linum-mode)

(provide 'elm-config)
;;; elm-config.el ends here
