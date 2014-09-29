;; Ibuffer http://www.emacswiki.org/emacs/IbufferMode
(require 'ibuffer)

;; Get rid of title and summary
(defadvice ibuffer-update-title-and-summary (after remove-column-titles)
  (save-excursion
    (set-buffer "*Ibuffer*")
    (toggle-read-only 0)
    (goto-char 1)
    (search-forward "-\n" nil t)
    (delete-region 1 (point))
    (let ((window-min-height 1)) 
      ;; save a little screen estate
      (shrink-window-if-larger-than-buffer))
    (toggle-read-only)))
(ad-activate 'ibuffer-update-title-and-summary)


(provide 'ibuffer-settings)






