;;; desktop-settings.el --- Desktop

;;; Commentary:
;;  Save desktop.

;;; Code:

(desktop-save-mode t)


(setq-default desktop-dirname "~/.emacs.d/cache"
              desktop-path (list desktop-dirname)
              desktop-base-file-name "emacs-desktop"
              desktop-save t)


(provide 'desktop-settings)
;;; desktop-settings.el ends here
