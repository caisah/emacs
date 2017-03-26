;;; docview-settings.el --- config for docview

;;; Commentary:
;;  No need to leave Emacs when viewing documents

;;; Code:
(with-eval-after-load 'doc-view
  (progn
    (message "doc-view loaded")
    (setq doc-view-continuous t)))


(provide 'docview-settings)
;;; docview-settings.el ends here
