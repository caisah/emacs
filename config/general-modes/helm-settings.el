;;; helm-settings.el --- Helm settings

;;; Commentary:
;; https://github.com/emacs-helm/helm

;;; Code:

;; load helm at startup
(helm-mode t)

;; one cannot live without helm swoop
(require 'helm-swoop)

;; ag is really fast
(require 'helm-ag)

;; use helm for xref
(require 'helm-xref)


(setq-default helm-quick-update t
              helm-split-window-in-side-p t
              helm-buffers-fuzzy-matching t
              helm-move-to-line-cycle-in-source t
              helm-ff-search-library-in-sexp t
              helm-scroll-amount 8
              helm-ff-file-name-history-use-recentf t

              helm-ag-command-option "-i"

              xref-show-xrefs-function 'helm-xref-show-xrefs

              helm-multi-swoop-edit-save t
              helm-swoop-split-direction 'split-window-vertically
              helm-swoop-use-line-number-face t
              helm-swoop-pre-input-function (lambda () ""))


(when (executable-find "curl")
  (setq-default helm-google-suggest-use-curl-p t))


;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x f") 'helm-recentf)
(global-set-key (kbd "C-x C-S-f") 'helm-projectile)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-.") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-x C-b") 'helm-resume)

;; show all key bindings using helm
(global-set-key (kbd "C-h b") 'helm-descbinds)

;; Swoop
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop-current-mode)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)


;; Projectile & Search
(global-set-key (kbd "C-x C-S-f") 'helm-projectile)
(global-set-key (kbd "C-x M-g") 'helm-projectile-ag)
(global-set-key (kbd "C-x g") 'helm-do-ag)


;; Helm Projectile
(setq projectile-mode-line " Projectile")
(setq projectile-completion-system 'helm)
(setq helm-projectile-fuzzy-match nil)


(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))


;; Export
(provide 'helm-settings)

;;; helm-settings.el ends here
