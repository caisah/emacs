;; Smart Mode Line https://github.com/Bruce-Connor/smart-mode-line

(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'dark)
(setq sml/mode-width 15)
(setq rm-excluded-modes
      (list " Anzu" " ARev" " SP/s" " SP" " Abbrev" " Isearch"
            " A" " Guide"  " Undo-Tree" " PgLn" " MRev"
            " skewer" " skewer-html" " skewer-css"" Emmet" " hs"
            " λ" " Rbow" " vl" " Wrap" " Helm" " Projectile" " yas"
            ))


(provide 'sml-settings)
