;; Httpd https://github.com/skeeto/emacs-web-server
(require 'simple-httpd)
(setq httpd-root "~/Documents/javascript/")

;; Smart Compile http://www.emacswiki.org/emacs/SmartCompile
(require 'smart-compile)

;; Undo Tree http://www.emacswiki.org/UndoTree
(require 'undo-tree)
(global-undo-tree-mode)

;; Expand Region https://github.com/magnars/expand-region.el
(require 'expand-region)

;; Anzu https://github.com/syohex/emacs-anzu
(require 'anzu)
(global-anzu-mode t)

;; Rainbow Delimiters https://github.com/jlr/rainbow-delimiters
(require 'rainbow-delimiters)

;; Re-Builder
(require 're-builder)
(setq reb-re-syntax 'string)

;; Page Break Lines https://github.com/purcell/page-break-lines
(require 'page-break-lines)
(global-page-break-lines-mode t)

;; Emmet https://github.com/smihica/emmet-mode
(require 'emmet-mode)
(setq emmet-preview-default nil) 

;; Multiple Cursors https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)

;; Org Mode
(require 'org)

;; Nginx mode https://github.com/ajc/nginx-mode
(require 'nginx-mode)


(provide 'othermodes-settings)
