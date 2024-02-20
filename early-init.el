(setq package-enable-at-startup nil)

(setq-default no-littering-etc-directory (expand-file-name ".litter/etc/" user-emacs-directory))
(setq-default no-littering-var-directory (expand-file-name ".litter/var/" user-emacs-directory))
(setq-default temporary-file-directory (expand-file-name ".litter/temp" user-emacs-directory))
