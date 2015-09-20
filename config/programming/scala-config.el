;;; scala-config.el --- Scala config

;;; Commentary:
;; My Scala config

;;; Code:
(require 'scala-mode2)
;; sbt
(require 'sbt-mode)

(add-hook 'scala-mode-hook 'linum-mode)
(add-hook 'scala-mode-hook 'smartparens-strict-mode)
(add-hook 'scala-mode-hook 'hs-minor-mode)
(add-hook 'scala-mode-hook 'flycheck-mode)
(add-hook 'scala-mode-hook 'whitespace-mode)

(add-hook 'scala-mode-hook '(lambda ()
   ;; sbt-find-definitions is a command that tries to find (with grep)
   ;; the definition of the thing at point.
   (local-set-key (kbd "M-.") 'sbt-find-definitions)
   (local-set-key (kbd "C-c C-e") 'sbt-command)

   ;; use sbt-run-previous-command to re-compile your code after changes
   (local-set-key (kbd "C-x '") 'sbt-run-previous-command)
))
;; Exports
(provide 'scala-config)
;;; scala-config.el ends here
