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
(setq projectile-mode-line " Projectile")
(setq projectile-completion-system 'helm)
(setq helm-projectile-fuzzy-match nil)

;; Helm Swoop
(require 'helm-swoop)
(setq-default helm-multi-swoop-edit-save t)
(setq-default helm-swoop-split-direction 'split-window-vertically)

(require 'helm-ag)
(setq helm-ag--ignore-case t)

(eval-after-load 'flycheck
   '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;; Export
(provide 'helm-settings)

;;; helm-settings.el ends here
