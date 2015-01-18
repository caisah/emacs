;;; helm-settings.el --- Helm settings

;;; Commentary:
;; https://github.com/emacs-helm/helm

;;; Code:
(require 'helm-config)
(require 'helm-grep)

(when (executable-find "curl")
  (setq-default helm-google-suggest-use-curl-p t))

(setq-default helm-quick-update t
              helm-split-window-in-side-p t
              helm-buffers-fuzzy-matching t
              helm-move-to-line-cycle-in-source t
              helm-ff-search-library-in-sexp t
              helm-scroll-amount 8
              helm-ff-file-name-history-use-recentf t)

(helm-mode t)

;; Helm Projectile
(require 'helm-projectile)
(setq projectile-mode-line " Projectile")

;; Helm Swoop
(require 'helm-swoop)
(setq-default helm-multi-swoop-edit-save t)
(setq-default helm-swoop-split-direction 'split-window-vertically)

(defun helm-do-grep-recursive (&optional non-recursive)
  "Like `helm-do-grep', but greps recursively by default."
  (interactive "P")
  (let* ((current-prefix-arg (not non-recursive))
         (helm-current-prefix-arg non-recursive))
    (call-interactively 'helm-do-grep)))

;; Export
(provide 'helm-settings)

;;; helm-settings.el ends here
