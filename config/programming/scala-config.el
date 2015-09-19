;;; scala-config.el --- Scala config

;;; Commentary:
;; My Scala config

;;; Code:
(require 'scala-mode2)
;; sbt
(require 'sbt-mode)

(add-hook 'scala-mode2-hook 'linum-mode)
(add-hook 'scala-mode2-hook 'smartparens-strict-mode)
(add-hook 'scala-mode2-hook 'hs-minor-mode)
(add-hook 'scala-mode2-hook 'flycheck-mode)
(add-hook 'scala-mode2-hook 'whitespace-mode)

;; Exports
(provide 'scala-config)
;;; scala-config.el ends here
