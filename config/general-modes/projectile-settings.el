;;; projectile-settings.el --- Projectile settings
;;; Commentary:
;; Settings for projectile

;;; Code:

;; Start projectile
(projectile-mode)

(setq-default
 ;; Use elisp
 projectile-indexing-method 'native
 ;; Cache projectile files
 projectile-enable-caching t
 ;; When switching a project switch to a dir
 projectile-switch-project-action #'projectile-find-dir)

(provide 'projectile-settings)
;;; projectile-settings ends here
