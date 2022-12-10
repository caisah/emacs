;;; projectile-settings.el --- Projectile settings
;;; Commentary:
;; Settings for projectile

;;; Code:

;; Use projectile package
(straight-use-package 'projectile)
(straight-use-package 'counsel-projectile)

(with-eval-after-load 'projectile
  (progn
    (message "My init :: projectile loaded")

    (setq
     ;; Use elisp
     projectile-indexing-method 'alien
     ;; Use ivy
     projectile-completion-system 'ivy
     ;; When switching a project switch to a dir
     projectile-switch-project-action #'projectile-dired)

    (define-key projectile-mode-map (kbd "s-h") 'projectile-command-map)))

;; Always use projectile
(projectile-mode 1)

(with-eval-after-load 'counsel-projectile
  (progn
    (message "My init :: counsel projectile loaded")

    (define-key projectile-mode-map (kbd "s-h g") 'projectile-ag)
))

;; Use counsel with projectile
(counsel-projectile-mode 1)

(provide 'projectile-settings)
;;; projectile-settings ends here
