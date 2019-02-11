;;; projectile-settings.el --- Projectile settings
;;; Commentary:
;; Settings for projectile

;;; Code:

;; Start projectile
(projectile-mode)

;; Use helm
(require 'helm-projectile)
(helm-projectile-on)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Use helm for switching buffers
;; (substitute-key-definition
;;  'projectile-switch-to-buffer 'helm-projectile-switch-to-buffer 'projectile-command-map)


(setq-default
 ;; Use elisp
 projectile-indexing-method 'alien
 ;; Cache projectile files
 projectile-enable-caching t
 ;; When switching a project switch to a dir
 projectile-switch-project-action #'projectile-dired)

(provide 'projectile-settings)
;;; projectile-settings ends here
