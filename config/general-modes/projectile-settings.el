;;; projectile-settings.el --- Projectile settings
;;; Commentary:
;; Settings for projectile

;;; Code:

;; Start projectile
(projectile-mode)

(setq-default
 ;; Use elisp
 projectile-indexing-method 'alien
 ;; Cache projectile files
 projectile-enable-caching t
 ;; When switching a project switch to a dir
 projectile-switch-project-action #'projectile-dired)

(provide 'projectile-settings)
;;; projectile-settings ends here
