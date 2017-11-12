;;; hippie-expand-settings.el --- config for hippie expand

;;; Commentary:
;;  Trying hippie expand

;;; Code:
(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-expand-line
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol))

(provide 'hippie-expand-settings)
;;; hippie-expand-settings.el ends here
