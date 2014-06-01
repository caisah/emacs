			    ;; On Startup
;; ========================================================================================
;; Start fullscreen
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
(toggle-fullscreen)

;; Varibales
(setq
 sml/mode-width 15)

;; Disable menu bar mode
(menu-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable scroll
(set-scroll-bar-mode nil)

;; Fringes
(set-fringe-mode '(1 . 1))

;; Show parens
(show-paren-mode t)

;; Show size of file
(size-indication-mode t)

;; Show column number
(column-number-mode t)

;; Lnaguage
(set-language-environment "UTF-8")

;; Theme
(load-theme 'liso t)

;; Split the window
(split-window-right)

 
			      ;; Colors:
;; ========================================================================================+
;; Set colors for term
(defface term-color-black 
  '((t (:foreground "#black" :background "#272C2E"))) 
  "Unhelpful docstring.")
(defface term-color-red
  '((t (:foreground "#E02831" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-green
  '((t (:foreground "#FFE203" :background "#3E2C2E")))
  "Unhelpful docstring.")
(defface term-color-yellow
  '((t (:foreground "#FFA826" :background "#272C2E"))) 
  "Unhelpful docstring.")
(defface term-color-blue 
  '((t (:foreground "#9E60B3" :background "#272C2E"))) 
  "Unhelpful docstring.")
(defface term-color-magenta
  '((t (:foreground "#A8B3FF" :background "#272C2E"))) 
  "Unhelpful docstring.")
(defface term-color-cyan
  '((t (:foreground "#E6A8DF" :background "#FFC2F8"))) 
  "Unhelpful docstring.")
(defface term-color-white
  '((t (:foreground "#EDEDE1" :background "#1F454D")))
  "Unhelpful docstring.")
'(term-default-fg-color ((t (:inherit term-color-white))))
'(term-default-bg-color ((t (:inherit term-color-black))))


;; Ansi Term

(setq ansi-term-color-vector
  [term term-color-black term-color-red term-color-blue term-color-yellow 
    term-color-green term-color-magenta term-color-cyan term-color-white])
(setq ansi-color-map (ansi-color-make-color-map))


;; Anzu color

(set-face-attribute 'anzu-mode-line nil
                    :foreground "PaleVioletRed2"
		    :weight 'bold)



;; Re-builder

(set-face-attribute 'reb-match-0 nil
		    :background "#B55100"
		    :foreground "#eeeeec"
		    :box '(:line-width 1 :color "#B3B381"))

(set-face-attribute 'reb-match-1 nil
		    :background "#754901"
		    :foreground "#eeeeec"
		    :box '(:line-width 1 :color "#B3B381"))


;; IDO

(set-face-attribute 'ido-first-match nil
                    :foreground "#F6FFD1"
		    :weight 'bold)

(set-face-attribute 'ido-only-match nil
                    :foreground "#B0E103"
		    :weight 'semi-bold)


;; Dired Plus

(set-face-attribute 'diredp-dir-priv nil
                    :foreground "#F5D902"
		    :weight 'bold)
(set-face-attribute 'diredp-file-name nil
                    :foreground "#9E60B3"
		    :weight 'semi-bold)
(set-face-attribute 'diredp-file-suffix nil
                    :foreground "#6E477A"
 		    :slant 'italic)
(set-face-attribute 'diredp-dir-heading nil
		    :background "#3E2C2E"
                    :foreground "#e6a8df"
		    :weight 'ultra-bold)
(set-face-attribute 'diredp-symlink nil
                    :foreground "#FFA8FF")
(set-face-attribute 'diredp-compressed-file-suffix nil
                    :foreground "#66421F")
(set-face-attribute 'diredp-flag-mark nil
                    :foreground "#F5D902"
		    :background "#272C2E")
(set-face-attribute 'diredp-flag-mark-line nil
		    :foreground "#131617"
		    :background "#A8B3FF"
		    )
(set-face-attribute 'diredp-ignored-file-name nil
                    :foreground "#5D6A70")
(set-face-attribute 'diredp-deletion-file-name nil
                    :foreground "#131617"
		    :background "#D9A3C7")
(set-face-attribute 'diredp-deletion nil
                    :foreground "red"
		    :background "#272C2E"		    
 		    :weight 'bold)
(set-face-attribute 'diredp-read-priv nil
                    :foreground "#8CC4E9"
		    :background "#272C2E"
		    :weight 'bold)
(set-face-attribute 'diredp-write-priv nil
                    :foreground "#B0E103"
		    :background "#272C2E"
		    :weight 'bold)
(set-face-attribute 'diredp-exec-priv nil
                    :foreground "#f57900"
		    :background "#272C2E"
 		    :weight 'bold)
(set-face-attribute 'diredp-date-time nil
                    :foreground "#5D6A70")
(set-face-attribute 'diredp-number nil
                    :foreground "#EDEDE1")
(set-face-attribute 'diredp-no-priv nil
                    :foreground "#5D6A70")

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
