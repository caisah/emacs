;;; twitter-settings.el --- loads settings

;;; Commentary:
;; Twittering Mode

;;; Code:

(require 'twittering-mode)

(defface twittering-user-name
  '((t (:weight bold)))
  "test face"
  :group 'faces)

(defface twittering-time
  '((t (:inherit (font-lock-comment-face))))
  "test face"
  :group 'faces)

(defface twittering-location
  '((t (:inherit (font-lock-comment-face))))
  "test face"
  :group 'faces)

(defface twittering-reply
  '((t (:inherit (font-lock-doc-face))))
  "test face"
  :group 'faces)

(defface twittering-delimiter
  '((t (:inherit (vertical-border))))
  "test face"
  :group 'faces)


(setq twittering-icon-mode t)
(setq twittering-use-master-password t)
(setq twittering-convert-fix-size 35)

(setq twittering-status-format "%i%FACE[twittering-user-name]{%s}%FACE[twittering-reply]{%r%R}%FACE[twittering-time]{ - %@}
%FILL{    %T}\n
%FILL{%FACE[twittering-location]{Client: %f -- Loc: %l}}
%FACE[twittering-delimiter]{---------------------------------------------}")

(defun twittering-open-with-eww ()
  "Open link from twitt with EWW."
  (interactive)
  (save-excursion
    (twittering-goto-next-uri)
    (let ((url (thing-at-point 'url)))
      (other-window 1)
      (eww url))))

(define-key twittering-mode-map (kbd "n") 'twittering-goto-next-status)
(define-key twittering-mode-map (kbd "p") 'twittering-goto-previous-status)
(define-key twittering-mode-map (kbd "j") 'twittering-visit-timeline)
(define-key twittering-mode-map (kbd "W") 'twittering-push-uri-onto-kill-ring)
(define-key twittering-mode-map (kbd "o") 'twittering-open-with-eww)
;; Export:
(provide 'twitter-settings)
;;; twitter-settings ends here
