;;; liso-theme.el Eclectic Dark Theme for GNU Emacs

;; Inspired by TangoTango Theme https://github.com/juba/color-theme-tangotango

;; Author: Vlad Piersec <vlad.piersec@gmail.com>
;; Keywords: theme, themes
;; URL: https://github.com/caisah/liso-theme
;; Version: 2.0

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

(let ((background "#272C2E")
      (foreground "#EDEDE1") 
      (cursor-yellow "#E6CB00")
      (foreground-black "#1A1A1A")
      (escape "#885D79")
      (prompt-green "#C8FF03")
      (highlight-yellow "#ECE185")
      (region-background "#31454F")
      (fail-dark-red "#8B0000")
      (error-red "#E02831")
      (warning "#FF7E00")
      (search-pink "#FFEDE6")
      (search-orange "#CE5C00")
      (search-brown "#795317") 
      (comment "#5D6A70")
      (liso-yellow "#FFE203")
      (liso-orange "#FF9326")
      (liso-pink "#FFABAB")
      (liso-purple "#B46DCC")
      (liso-red "#C04040")
      (liso-green "#C8FF03")
      (liso-dark-green "#6E8C02")
      (liso-blue "#99D6FF")
      (link-blue "#7CA4CF")
      (link-dark-blue "#5075A4")
      (ml-black "#131617")
      (ml-yellow "#EAB700")
      (ml-grey "#C7C7AB")
      (ml-grey-darker "#414B4E")
      (paren-blue "#7AD9FF")
      (paren-red "#9D005C"))

  (custom-theme-set-faces
   'liso
   
   `(default ((t (:family "Ubuntu Mono"
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
                  :foreground ,foreground 
                  :background ,background
                  :stipple nil
                  :inherit nil))))
   `(cursor ((t (:foreground ,foreground-black :background ,cursor-yellow)))) 
   `(fixed-pitch ((t (:inherit (default))))) 
   `(variable-pitch ((t (:family "Sans Serif")))) 
   `(escape-glyph ((t (:foreground ,escape)))) 
   `(minibuffer-prompt ((t (:weight normal :foreground ,prompt-green)))) 
   `(highlight ((t (:foreground ,foreground-black :background ,highlight-yellow)))) 
   `(region ((t (:background ,region-background)))) 
   `(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50"))
             (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70"))
             (((class color) (min-colors 8) (background light)) (:foreground "green"))
             (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
   `(secondary-selection ((t (:background ,search-brown))))
   `(trailing-whitespace ((t (:background ,fail-dark-red))))
   `(whitespace-trailing ((t (:background ,fail-dark-red :foreground ,foreground-black))))
   `(font-lock-builtin-face ((t (:foreground ,liso-purple))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-constant-face ((t (:foreground ,liso-red))))
   `(font-lock-doc-face ((t (:foreground ,liso-dark-green))))
   `(font-lock-function-name-face ((t (:foreground ,liso-yellow))))
   `(font-lock-keyword-face ((t (:foreground ,liso-green))))
   `(font-lock-negation-char-face ((t nil))) 
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face))))) 
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold))))) 
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground ,liso-pink))))
   `(font-lock-type-face ((t (:foreground ,liso-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,liso-orange))))
   `(font-lock-warning-face ((t (:weight bold :foreground ,warning :inherit (error)))))
   `(button ((t (:inherit (link)))))
   `(link ((t (:underline (:color foreground-color :style line) :foreground ,link-blue)))) 
   `(link-visited ((t (:underline (:color foreground-color :style line) :foreground ,link-dark-blue :inherit (link)))))
   `(fringe ((t (:background ,background))))
   `(header-line ((t (:foreground ,comment :weight bold)))) 
   `(tooltip ((t (:foreground "black" :background "lightyellow" :inherit (quote variable-pitch)))))
   `(mode-line ((t (:box (:line-width -1 :color nil :style pressed-button) :foreground "white smoke" :background ,ml-black)))) 
   `(mode-line-buffer-id ((t (:weight bold :foreground ,ml-yellow)))) 
   `(mode-line-emphasis ((t (:weight bold)))) 
   `(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight))))) 
   `(mode-line-inactive ((t (:weight light :box (:line-width -1 :color nil :style released-button) :foreground ,ml-grey :background ,ml-grey-darker :inherit (mode-line))))) 
   `(isearch ((t (:foreground ,search-pink :background ,search-orange :underline (:color foreground-color :style line)))))
   `(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1"))
                   (((class color) (min-colors 88) (background dark)) (:background "red4"))
                   (((class color) (min-colors 16)) (:background "red"))
                   (((class color) (min-colors 8)) (:background "red"))
                   (((class color grayscale)) (:foreground "grey"))
                   (t (:inverse-video t)))) 
   `(anzu-mode-line ((t (:foreground "PaleVioletRed2" :weight bold))))
   `(lazy-highlight ((t (:foreground ,foreground-black :background ,search-brown)))) 
   `(match ((t (:weight bold :foreground ,search-orange :background ,foreground-black))))
   `(next-error ((t (:inherit (region))))) 
   `(query-replace ((t (:inherit (isearch))))) 
   `(show-paren-match ((t (:background ,paren-blue :foreground ,background))))
   `(show-paren-mismatch ((t (:background ,paren-red :foreground "red"))))
   `(linum ((t (:foreground "#6F8085" :weight light :height 0.9))))
   `(vertical-border ((t (:foreground ,ml-black)))) 
   `(error ((t (:foreground ,error-red :weight semi-bold)))) 
   `(completions-first-difference ((t (:inherit (highlight))))) 
   `(diredp-dir-priv ((t (:foreground ,liso-yellow :weight bold))))
   `(diredp-file-name ((t (:foreground ,foreground :weight normal))))
   `(diredp-file-suffix ((t (:foreground ,comment :slant italic))))
   `(diredp-dir-heading ((t (:background ,background :foreground ,ml-yellow :weight ultra-bold))))
   `(diredp-symlink ((t (:foreground ,link-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,foreground))))
   `(diredp-flag-mark ((t (:background ,liso-yellow :foreground ,background))))
   `(diredp-flag-mark-line ((t (:inherit (diredp-flag-mark)))))
   `(diredp-ignored-file-name ((t (:foreground ,comment))))
   `(diredp-deletion-file-name ((t (:background ,liso-red :foreground ,foreground))))
   `(diredp-deletion ((t (:inherit (diredp-deletion-file-name)))))
   `(diredp-read-priv ((t (:background ,background :foreground ,foreground :weight bold))))
   `(diredp-write-priv ((t (:background ,background :foreground ,liso-dark-green :weight bold))))
   `(diredp-exec-priv ((t (:background ,background :foreground ,liso-green :weight bold))))
   `(diredp-link-priv ((t (:background ,background :foreground ,liso-orange :weight bold))))
   `(diredp-date-time ((t (:foreground ,comment))))
   `(diredp-number ((t (:foreground ,comment))))
   `(diredp-no-priv ((t (:foreground ,foreground))))
   
   `(helm-action ((t (:foreground ,foreground :underline nil))))
   `(helm-selection ((t (:foreground ,paren-red :background ,highlight-yellow :weight bold))))
   `(helm-source-header ((t (:background: ,background :foreground ,ml-yellow :family "Ubuntu Mono" :weight normal :height: 1.1))))
   `(helm-visible-mark ((t (:inherit (diredp-flag-mark)))))
   `(helm-candidate-number ((t (:inherit (match)))))
   `(helm-buffer-directory ((t (:inherit (diredp-dir-priv)))))
   `(helm-buffer-size ((t (:foreground ,comment))))
   `(helm-buffer-process ((t (:inherit (font-lock-doc-face)))))
   `(helm-ff-directory ((t (:inherit (diredp-dir-priv)))))
   `(helm-ff-file ((t (:inherit (diredp-file-name)))))
   `(helm-ff-symlink ((t (:inherit (diredp-symlink)))))
   `(helm-M-x-key ((t (:foreground ,liso-red :weight bold))))
   `(helm-match ((t (:foreground ,liso-green))))
   `(helm-separator ((t (:foreground ,liso-dark-green))))

   )

  )

;; Ibuffer
(setq ibuffer-marked-face 'diredp-flag-mark)
(setq ibuffer-deletion-face 'diredp-deletion-file-name)

;;; Other cusomizations
;; Disable menu bar mode
(menu-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable scroll
(set-scroll-bar-mode nil)

;; Fringes
(set-fringe-mode '(1 . 1))

;; Show size of file
(size-indication-mode t)

;; Show column number
(column-number-mode t)


(provide-theme 'liso)
