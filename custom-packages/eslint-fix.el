(defvar eslint-fix-command nil)

(defun eslint-fix--str-concat (str1 str2)
  "Concat STR1 and STR2."
  (concat str1 " " str2))

(defun eslint-fix-command ()
  "Try to find the eslint command."
  (or
   eslint-fix-command

    (let ((root (locate-dominating-file buffer-file-name "node_modules")))
      (if root
          (let ((command (concat root "node_modules/.bin/eslint")))
            (if (file-executable-p command) command))))
    (executable-find "eslint")

    (error "Couldn't find a eslint executable")))



(defun eslint-fix--full-command ()
  "Create full command for eslint fix execution."
  (cl-reduce 'eslint-fix--str-concat (list (eslint-fix-command) buffer-file-name "--fix")))

(defun eslint-fix ()
  "Run eslint fix on the buffer file."
  (interactive)
  (shell-command (eslint-fix--full-command)  "*eslint-fix output*" "*eslint-fix error")
  (revert-buffer t t t))


;;;###autoload
(define-minor-mode eslint-fix-mode
  "Runs eslint fix on file save when this mode is turned on"
  :global nil
  :lighter " ESFix"
  (if eslint-fix-mode
      (add-hook 'after-save-hook 'eslint-fix nil 'local)
    (remove-hook 'after-save-hook 'eslint-fix 'local)))

(provide 'eslint-fix)

;;; eslint-fix.el ends here
