;;; clojure-config.el --- My clojure config

;;; Commentary:
;; https://github.com/clojure-emacs/cider

;;; Code:
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq-default nrepl-log-messages t)
(setq-default cider-prefer-local-resources t)
(setq-default cider-repl-pop-to-buffer-on-connect nil)
(setq-default cider-repl-result-prefix ";; => ")
(setq-default cider-repl-wrap-history t)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode 'rainbow-delimiters-mode)


(provide 'clojure-config)
;;; clojure-config ends here
