;;; liso-theme.el Eclectic Dark Theme for GNU Emacs

;; Inspired by TangoTango Theme https://github.com/juba/color-theme-tangotango

;; Author: Vlad Piersec <vlad.piersec@gmail.com>
;; Keywords: theme, themes
;; URL: https://github.com/caisah/liso-theme
;; Version: 1.0

;;; License:
 
;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Code

(deftheme liso
  "Liso - Eclectic Dark Theme inspired by TangoTango")

(custom-theme-set-faces
 'liso
 '(default ((t (:family "Ubuntu Mono"
		:foundry "unknown"
		:width normal
		:height 128
		:weight normal
		:slant normal
		:underline nil
		:overline nil
		:strike-through nil
		:box nil
		:inverse-video nil
		:foreground "#EDEDE1"
		:background "#272C2E"
		:stipple nil
		:inherit nil))))

 '(cursor ((t (:foreground "#1A1A1A"
	       :background "#E6CB00"))))
 
 '(fixed-pitch ((t (:family "Monospace"
		    :inherit (default)))))
 
 '(variable-pitch ((t (:family "Sans Serif"))))
 
 '(escape-glyph ((t (:foreground "#7D5559"))))
 
 '(minibuffer-prompt ((t (:weight normal
			  :foreground "#C8FF03"))))

 '(highlight ((t (:foreground "#272C2E"
		  :background "#E5D86D"))))
 
 '(region ((t (:background "#1F454D"))))
 
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50"))
	   (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70"))
	   (((class color) (min-colors 8) (background light)) (:foreground "green"))
	   (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 
 '(secondary-selection ((t (:background "#204A87"))))
 
 '(trailing-whitespace ((t (:background "#A40000"))))
 
 '(font-lock-builtin-face ((t (:foreground "#9E60B3"))))
 
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 
 '(font-lock-comment-face ((t (:foreground "#5D6A70"))))
 
 '(font-lock-constant-face ((t (:foreground "#FFC2F8"))))
 
 '(font-lock-doc-face ((t (:foreground "#537A30"
		           :inherit (font-lock-string-face)))))
 
 '(font-lock-function-name-face ((t (:foreground "#FFE203"))))
 
 '(font-lock-keyword-face ((t (:foreground "#C8FF03"))))
 
 '(font-lock-negation-char-face ((t nil)))
 
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 
 '(font-lock-string-face ((t (:foreground "#C9A296"))))
 
 '(font-lock-type-face ((t (:foreground "#99D6FF"))))
 
 '(font-lock-variable-name-face ((t (:foreground "#FFA826"))))
 
 '(font-lock-warning-face ((t (:weight bold
			       :foreground "#F57900"
			       :inherit (error)))))
 
 '(button ((t (:inherit (link)))))
 
 '(link ((t (:underline (:color foreground-color
			 :style line)
	     :foreground "#729FCF"))))
 
 '(link-visited ((t (:underline (:color foreground-color
				 :style line)
		     :foreground "#3465A4"
		     :inherit (link)))))
 
 '(fringe ((t (:background "#272C2E"))))
 
 '(header-line ((t (:foreground "#E6A8DF"
		    :weight ultra-bold))))
 
 '(tooltip ((t (:foreground "black"
	        :background "lightyellow"
		:inherit (quote variable-pitch)))))
 
 '(mode-line ((t (:box (:line-width -1 :color nil :style pressed-button)
		  :foreground "white smoke"
		  :background "#131617"))))
 
 '(mode-line-buffer-id ((t (:weight bold
			    :foreground "#EAB700"))))
 
 '(mode-line-emphasis ((t (:weight bold))))
 
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2
								:color "grey40"
								:style released-button)))
			(t (:inherit (highlight)))))
 
 '(mode-line-inactive ((t (:weight light
		           :box (:line-width -1 :color nil :style released-button)
			   :foreground "#C7C7AB"
			   :background "#414B4E"
			   :inherit (mode-line)))))
 
 '(isearch ((t (:foreground "#FFEDE6"
		:background "#CE5C00"
		:underline (:color foreground-color
				 :style line)))))
 
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1"))
		 (((class color) (min-colors 88) (background dark)) (:background "red4"))
		 (((class color) (min-colors 16)) (:background "red"))
		 (((class color) (min-colors 8)) (:background "red"))
		 (((class color grayscale)) (:foreground "grey"))
		 (t (:inverse-video t))))
 
 '(lazy-highlight ((t (:foreground "#231806"
				   :background "#795317"))))
 
 '(match ((t (:weight bold
	      :foreground "#2E3436"
	      :background "#E9B96E"))))
 
 '(next-error ((t (:inherit (region)))))
 
 '(query-replace ((t (:inherit (isearch)))))

 '(show-paren-match ((t (:background "#7AD9FF"
			 :foreground "#272C2E"))))
 
 '(show-paren-mismatch ((t (:background "#9D005C"
			    :foreground "red"))))

 '(dired-directory ((t (:foreground "#FFE203"
			:weight bold))))
 
 '(dired-header ((t (:foreground "#E6A8DF"
		     :background "#3E2C2E"
		     :weight ultra-bold))))

 '(dired-symlink ((t (:foreground "#FFA8FF"))))
 
 '(dired-marked ((t (:foreground "#131617"
		     :background "#A8B3FF"))))

 '(dired-flagged ((t (:foreground "#131617"
		      :background "#D9A3C7"))))
  
 '(dired-mark ((t (:foreground "#FFE203"))))

 '(dired-ignored ((t (:foreground "#5D6A70"))))

 '(dired-perm-write ((t (:foreground "#E6A8DF"
		         :weight ultra-bold))))
 
 '(linum ((t (:foreground "#6F8085"
	      :weight light
	      :height 0.9))))

 '(vertical-border ((t (:foreground "#131617"))))

 '(error ((t (:foreground "#E02831"
	       :weight semi-bold))))
 

 '(completions-first-difference ((t (:foreground "#FFA826"
				     :weight bold
				     :underline t)))))

;; Shell colors
(setq ansi-color-names-vector ["#131617" "#E02831" "#A8B3FF" "#FFA826" 
			       "#FFE203" "#99D6FF" "#FFC2F8" "#EDEDE1"])

;; Autoload for MELPA

;;;###autoload
(and load-file-name
  (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))
 
(provide-theme 'liso)

;;; liso-theme.el ends here
