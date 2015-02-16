;;; flycheck-settings.el --- Flycheck settings

;;; Commentary:
;; Flycheck https://github.com/flycheck/flycheck

;;; Code:
(require 'flycheck)

(defun enable-jshint ()
  "Enable jshint."
  (setq flycheck-jshintrc "~/.emacs.d/external/.jshintrc")
  (setq flycheck-javascript-jshint-executable "jshint"))

;; Flycheck for web-mode
(flycheck-define-checker html-tidy
  "A HTML syntax and style checker using Tidy.
See URL `https://github.com/w3c/tidy-html5'."
  :command ("tidy" (config-file "-config" flycheck-tidyrc) "-e" "-q" source)
  :error-patterns
  ((error line-start
          "line " line
          " column " column
          " - Error: " (message) line-end)
   (warning line-start
            "line " line
            " column " column
            " - Warning: " (message) line-end))
  :modes (html-mode nxhtml-mode web-mode))

;; Flycheck for typescript-mode
(flycheck-define-checker typescript
  "A TypeScript syntax checker using tsc command."
  :command ("tsc" "--out" "/dev/null" source)
  :error-patterns
  ((error line-start (file-name) "(" line "," column "): error " (message) line-end))
  :modes (typescript-mode))

;; Flycheck for JSX (react.js)
(flycheck-define-checker jsx
  "A JSX syntax checker using tsc command."
  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (jsx-mode))


;; Add checkers
(add-to-list 'flycheck-checkers 'typescript)
(add-to-list 'flycheck-checkers 'jsx)

(provide 'flycheck-settings)
;;; flycheck-settings.el ends here
