;;; early-init.el ---   -*- lexical-binding:t -*-

;; no need for package enabled at startup
(setq package-enable-at-startup nil)

(setq-default
 no-littering-etc-directory (expand-file-name ".litter/etc/" user-emacs-directory)
 no-littering-var-directory (expand-file-name ".litter/var/" user-emacs-directory)
 temporary-file-directory (expand-file-name ".litter/temp" user-emacs-directory))

(when (boundp 'native-comp-eln-load-path)
  (setopt native-comp-eln-load-path
          (cons (expand-file-name ".litter/var/eln-cache/" user-emacs-directory)
                (cdr native-comp-eln-load-path))))

;;; early-init.el ends here
