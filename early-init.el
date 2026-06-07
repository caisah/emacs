;;; early-init.el ---   -*- lexical-binding:t -*-

;;; Commentary:
;;; Runs this before anything else.
;;; Code:

(setq package-enable-at-startup nil)
(setq-default
 no-littering-etc-directory (expand-file-name ".litter/etc/" user-emacs-directory)
 no-littering-var-directory (expand-file-name ".litter/var/" user-emacs-directory)
 temporary-file-directory (expand-file-name ".litter/temp" user-emacs-directory))

;;; early-init.el ends here
