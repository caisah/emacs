;;; ivy-settings.el --- Ivy

;;; Commentary:
;; Be fast with Ivy

;;; Code:
;; Use ivy
(ivy-mode 1)
;; Use counsel
(counsel-mode 1)

;; Yes, use virtual buffers
(setq ivy-use-virtual-buffers t)
;; Format the count
(setq ivy-count-format "%d/%d ")
;; Continue scrolling at the top when hitting the end.
(setq ivy-wrap t)
;; Make ivy larger
(setq ivy-height 20)

;; Counsel
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-p") 'counsel-command-history)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c f") 'counsel-recentf)
(global-set-key (kbd "C-x M-i") 'counsel-ibuffer)

;; Swiper
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-i") 'swiper-thing-at-point)
(global-set-key (kbd "M-o") 'swiper-avy)

;; avy
(global-set-key (kbd "C-'") 'ivy-avy)

;; Ivy
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

(provide 'ivy-settings)
;;; ivy-settings.el ends here