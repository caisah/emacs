			    ;; On Startup
;; ========================================================================================

;; Varibales



;; Lnaguage


;; Theme


			      ;; Colors:
;; ========================================================================================+
;; ;; Set colors for term
;; (defface term-color-black 
;;   '((t (:foreground "#black" :background ,background))) 
;;   "Unhelpful docstring.")
;; (defface term-color-red
;;   '((t (:foreground ,red-1 :background "#272822")))
;;   "Unhelpful docstring.")
;; (defface term-color-green
;;   '((t (:foreground ,yellow-4 :background ,brown-3)))
;;   "Unhelpful docstring.")
;; (defface term-color-yellow
;;   '((t (:foreground ,orange-2 :background ,background))) 
;;   "Unhelpful docstring.")
;; (defface term-color-blue 
;;   '((t (:foreground ,purple-1 :background ,background))) 
;;   "Unhelpful docstring.")
;; (defface term-color-magenta
;;   '((t (:foreground ,blue-light-3 :background ,background))) 
;;   "Unhelpful docstring.")
;; (defface term-color-cyan
;;   '((t (:foreground ,purple-2 :background ,purple-3))) 
;;   "Unhelpful docstring.")
;; (defface term-color-white
;;   '((t (:foreground "#EDEDE1" :background "#1F454D")))
;;   "Unhelpful docstring.")
;; '(term-default-fg-color ((t (:inherit term-color-white))))
;; '(term-default-bg-color ((t (:inherit term-color-black))))


;; ;; Ansi Term
;; 
;; (setq ansi-term-color-vector
;;   [term term-color-black term-color-red term-color-blue term-color-yellow 
;;     term-color-green term-color-magenta term-color-cyan term-color-white])
;; (setq ansi-color-map (ansi-color-make-color-map))


;; Anzu color

;; (set-face-attribute 'anzu-mode-line nil
;;                     :foreground "PaleVioletRed2"
;; 		    :weight 'bold)



;; Re-builder

;; (set-face-attribute 'reb-match-0 nil
;; 		    :background ,brown-4
;; 		    :foreground ,white-1
;; 		    :box '(:line-width 1 :color ,yellow-grey-2))

;; (set-face-attribute 'reb-match-1 nil
;; 		    :background "#754901"
;; 		    :foreground "#eeeeec"
;; 		    :box '(:line-width 1 :color "#B3B381"))


;; IDO

;; (set-face-attribute 'ido-first-match nil
;;                     :foreground "#F6FFD1"
;; 		    :weight 'bold)

;; (set-face-attribute 'ido-only-match nil
;;                     :foreground "#B0E103"
;; 		    :weight 'semi-bold)


;; Dired Plus

;; (set-face-attribute 'diredp-dir-priv nil
;;                     :foreground "#F5D902"
;; 		    :weight 'bold)
;; (set-face-attribute 'diredp-file-name nil
;;                     :foreground ,purple-1
;; 		    :weight 'semi-bold)
;; (set-face-attribute 'diredp-file-suffix nil
;;                     :foreground "#6E477A"
;;  		    :slant 'italic)
;; (set-face-attribute 'diredp-dir-heading nil
;; 		    :background ,brown-3
;;                     :foreground ,purple-2
;; 		    :weight 'ultra-bold)

;; (set-face-attribute 'diredp-symlink nil
;;                     :foreground ,purple-5)

;; (set-face-attribute 'diredp-compressed-file-suffix nil
;;                     :foreground "#66421F")
;; (set-face-attribute 'diredp-flag-mark nil
;;                     :foreground "#F5D902"
;; 		    :background ,background)
;; (set-face-attribute 'diredp-flag-mark-line nil
;; 		    :foreground ,black-2
;; 		    :background ,blue-light-3
;; 		    )
;; (set-face-attribute 'diredp-ignored-file-name nil
;;                     :foreground ,grey-1)
;; (set-face-attribute 'diredp-deletion-file-name nil
;;                     :foreground ,black-2
;; 		    :background ,purple-6)
;; (set-face-attribute 'diredp-deletion nil
;;                     :foreground "red"
;; 		    :background ,background		    
;;  		    :weight 'bold)
;; (set-face-attribute 'diredp-read-priv nil
;;                     :foreground "#8CC4E9"
;; 		    :background ,background
;; 		    :weight 'bold)
;; (set-face-attribute 'diredp-write-priv nil
;;                     :foreground "#B0E103"
;; 		    :background ,background
;; 		    :weight 'bold)
;; (set-face-attribute 'diredp-exec-priv nil
;;                     :foreground ,orange-3
;; 		    :background ,background
;;  		    :weight 'bold)
;; (set-face-attribute 'diredp-date-time nil
;;                     :foreground ,grey-1)
;; (set-face-attribute 'diredp-number nil
;;                     :foreground ,text)
;; (set-face-attribute 'diredp-no-priv nil
;;                     :foreground ,grey-1)

;; HTML

(set-face-attribute 'web-mode-html-tag-face nil
		    :foreground "#7990AD"
		    :weight 'normal)
(set-face-attribute 'web-mode-html-tag-bracket-face nil
		    :foreground "#7990AD"
		    :weight 'normal)
(set-face-attribute 'web-mode-html-attr-name-face nil
		    :foreground "#AD6C6C"
		    :weight 'normal)
(set-face-attribute 'web-mode-html-attr-value-face nil
		    :foreground "#9679AD"
		    :weight 'normal
		    :slant 'italic)
(set-face-attribute 'web-mode-block-delimiter-face nil
		    :foreground "brown4"
		    :weight 'normal)


