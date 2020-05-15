;;; projectile-settings.el --- Projectile settings
;;; Commentary:
;; Settings for projectile

;;; Code:

(with-eval-after-load 'projectile
  (progn
    (message "My init :: projectile loaded")

    (setq-default
     ;; Use elisp
     projectile-indexing-method 'alien
     ;; Use ivy
     projectile-completion-system 'ivy
     ;; Cache projectile files
     projectile-enable-caching t
     ;; When switching a project switch to a dir
     projectile-switch-project-action #'projectile-dired)

    (define-key projectile-mode-map (kbd "s-h") 'projectile-command-map)))

;; Always use projectile
(projectile-mode 1)

;; Use counsel with projectile
(counsel-projectile-mode 1)

(provide 'projectile-settings)
;;; projectile-settings ends here
