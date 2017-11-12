;;; dired-settings.el --- Calendar

;;; Commentary:
;; Calendar configuration

;;; Code:
(setq-default european-calendar-style 't
              calendar-week-start-day 1
              holiday-local-holidays '((holiday-fixed 1 1 "Anul Nou")
                                       (holiday-fixed 1 2 "A doua zi de anul nou")
                                       (holiday-fixed 1 24 "Ziua Unirii Principatelor Române")
                                       (holiday-fixed 4 1 "Ziua Internațională a Muncii")
                                       (holiday-fixed 6 1 "Ziua Copilului")
                                       (holiday-fixed 8 15 "Adormirea Maicii Domnului")
                                       (holiday-fixed 11 30 "Sfântul Andrei")
                                       (holiday-fixed 12 1 "Ziua Națională a României")
                                       (holiday-fixed 12 25 "Crăciunul")
                                       (holiday-fixed 12 26 "A doua zi de Crăciun")))

(provide 'calendar-settings)
;;; calendar-settings.el ends here
