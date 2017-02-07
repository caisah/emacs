;;; rust-config.el --- Rust config

;;; Commentary:
;; My Rust config

;;; Code:
(with-eval-after-load 'rust-mode
  (message "Rust loaded")
  )

(add-hook 'rust-mode-hook 'linum-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)


(provide 'rust-config)
;;; rust-config.el ends here
