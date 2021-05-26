;;; ivy-settings.el --- Ivy

;;; Commentary:
;; Be fast with Ivy

;;; Code:
;; Use ivy
(ivy-mode 1)
;; Use counsel
(counsel-mode 1)
;; Use avy
(avy-setup-default)

;; Yes, use virtual buffers
(setq-default ivy-use-virtual-buffers t)
;; Format the count
(setq-default ivy-count-format "%d/%d ")
;; Continue scrolling at the top when hitting the end.
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
(global-set-key (kbd "C-c f") 'counsel-recentf)
(global-set-key (kbd "C-x M-i") 'counsel-ibuffer)

;; Swiper
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-i") 'swiper-thing-at-point)

;; avy
(global-set-key (kbd "M-o") 'avy-goto-word-1)

;; Ivy
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; Customize ivy-rich
(setq-default ivy-rich-display-transformers-list
              '(ivy-switch-buffer
                (:columns
                 ((ivy-switch-buffer-transformer (:width 0.35))
                  (ivy-rich-switch-buffer-size (:width 7 :face ivy-separator))
                  (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                  (ivy-rich-switch-buffer-major-mode (:width 12 :face ivy-separator))
                  (ivy-rich-switch-buffer-project (:width 0.18 :face ivy-minibuffer-match-face-1))
                  (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))) :face ivy-virtual)))
                 :predicate
                 (lambda (cand) (get-buffer cand)))

                counsel-find-file
                (:columns
                 ((ivy-read-file-transformer)
                  (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))

                counsel-M-x
                (:columns
                 ((counsel-M-x-transformer (:width 0.4))
                  (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))

                counsel-describe-function
                (:columns
                 ((counsel-describe-function-transformer (:width 0.4))
                  (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))

                counsel-describe-variable
                (:columns
                 ((counsel-describe-variable-transformer (:width 0.4))
                  (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))

                counsel-recentf
                (:columns
                 ((ivy-rich-candidate (:width 0.8))
                  (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))

                counsel-bookmark
                (:columns ((ivy-rich-bookmark-type (:face font-lock-comment-face))
                           (ivy-rich-bookmark-info)))

                package-install
                (:columns
                 ((ivy-rich-candidate (:width 30))
                  (ivy-rich-package-version (:width 16 :face font-lock-comment-face))
                  (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
                  (ivy-rich-package-install-summary (:face font-lock-doc-face))))))

;; Ivy rich
(require 'ivy-rich)
(ivy-rich-mode 1)


(provide 'ivy-settings)
;;; ivy-settings.el ends here
