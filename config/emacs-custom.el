;;; custom.el --- Custom file
;;; Commentary:
;; Custom Emacs file

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(helm-ag-command-option "-i")
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (company-tern helm-company yasnippet yaml-mode web-mode undo-tree twittering-mode tuareg tss tern swiper solarized-theme smartparens smart-mode-line smart-compile slime skewer-mode scss-mode restclient rainbow-mode rainbow-delimiters queue quack pyvenv pretty-symbols popwin pandoc-mode pallet palette page-break-lines nginx-mode multiple-cursors markdown-mode magit list-processes+ less-css-mode keyfreq jsx-mode js-comint inf-mongo impatient-mode imenu-anywhere idomenu highlight-indentation helm-swoop helm-projectile helm-package helm-google helm-flycheck helm-ag haskell-mode google-translate gist geiser flycheck-ocaml flycheck-elm find-file-in-project expand-region exec-path-from-shell emmet-mode elm-mode elfeed dired+ deferred dash-functional company browse-kill-ring anzu)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
