;;; rust-config.el --- Rust config

;;; Commentary:
;; My Rust config

;;; Code:
(with-eval-after-load 'rust-mode
  (message "Rust loaded")

  (setq rust-playground-basedir "~/.emacs.d/cache"))



(add-hook 'rust-mode-hook 'linum-mode)
(add-hook 'rust-mode-hook (lambda ()
                            (flycheck-mode)
                            (flycheck-rust-setup)))


(provide 'rust-config)
;;; rust-config.el ends here