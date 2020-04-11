;;; ivy-settings.el --- Ivy

;;; Commentary:
;; Be fast with Ivy

;;; Code:
;; Use ivy
(ivy-mode 1)
;; Use counsel
(counsel-mode 1)

;; Yes, use virtual buffers
(setq-default ivy-use-virtual-buffers t)
;; Format the count
(setq-default ivy-count-format "%d/%d ")
;; Continue scrolling at the top when hidding the end.
(setq-default ivy-wrap t)
;; Make ivy larger
(setq-default ivy-height 20)

;; Counsel
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-p") 'counsel-command-history)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)

;; Swiper
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-i") 'swiper-all-thing-at-point)
(global-set-key (kbd "M-o") 'swiper-avy)

;; Ivy
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

(provide 'ivy-settings)
;;; ivy-settings.el ends here
