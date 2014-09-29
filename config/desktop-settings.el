;; Desktop http://www.emacswiki.org/DeskTop
(require 'desktop) 
(desktop-save-mode 1)

(setq desktop-dirname "~/.emacs.d/cache")
(setq desktop-path (list desktop-dirname))
(setq desktop-base-file-name "emacs-desktop")
(setq-default indent-tabs-mode nil)

(defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (if (eq (desktop-owner) (emacs-pid))
        (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)


(provide 'desktop-settings)
