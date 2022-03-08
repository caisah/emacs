(defvar eslint-fix-command nil)
(defvar eslint-fix-config nil)

(defun eslint-fix--str-concat (str1 str2)
  "Concat STR1 and STR2."
  (concat str1 " " str2))


(defun eslint-fix--default-dir ()
  "Try to find the eslint command."
  (let ((root (locate-dominating-file buffer-file-name "node_modules")))
    (if root root (error "Could not find node_modules dir"))))


(defun eslint-fix--config ()
  "Try to find the eslint command."
  (or
   eslint-fix-config

   (let* ((root (eslint-fix--default-dir))
          (config (concat root ".eslintrc.js")))

     (if (file-exists-p "~/Work/text-emojis/.eslintrc.js") config
       (error "Config file not found")))))

(defun eslint-fix-command ()
  "Try to find the eslint command."
  (or
   eslint-fix-command

   (let* ((root (eslint-fix--default-dir))
          (command (concat root "node_modules/.bin/eslint")))
     (if (file-executable-p command)
         command
       (error "Could not find eslint executable")))))

(defun eslint-fix--full-command ()
  "Create full command for eslint fix execution."
  (cl-reduce 'eslint-fix--str-concat (list (eslint-fix-command)  buffer-file-name "-c" (eslint-fix--config) "--fix")))

(defun eslint-fix ()
  "Run eslint fix on the buffer file."
  (interactive)
  (let ((default-directory "/Users/vlad/Work/text-emojis"))
    (shell-command (eslint-fix--full-command)  "*eslint-fix output*" "*eslint-fix error"))
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
