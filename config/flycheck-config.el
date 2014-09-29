;; Flycheck https://github.com/flycheck/flycheck
(require 'flycheck)

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


(provide 'flycheck-config)
