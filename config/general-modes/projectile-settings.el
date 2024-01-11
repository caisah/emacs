;;; projectile-settings.el --- Projectile settings
;;; Commentary:
;; Settings for projectile

;;; Code:

;; Use projectile package
(use-package projectile
  :straight t

  :init
  (projectile-mode 1)

  :config
  (message "My init :: projectile loaded")
  (setq
     ;; Use elisp
     projectile-indexing-method 'alien

     ;; When switching a project switch to a dir
     projectile-switch-project-action #'projectile-dired)

  :bind (:map projectile-mode-map
              ("s-h" . projectile-command-map)
              ("s-h g" . consult-ripgrep)))

(provide 'projectile-settings)
;;; projectile-settings ends here
