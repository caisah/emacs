;;; paleolithic-theme.el --- Dark Gray Theme

;; Author: Vlad Piersec <vlad.piersec@gmail.com>
;; Keywords: theme, themes, gray, grey, dark
;; Version: 0.3

;;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:
;; This is a very opinionated theme.

;;; Code:

(deftheme paleolithic
 "A Dark Gray Theme")
  (custom-theme-set-faces
   'paleolithic
   `(default ((t (:background "gray10" :foreground "gray80"))))
   `(cursor ((t (:background "chartreuse3" :foreground "black"))))
   `(region ((t (:background "gray20" :foreground "gray90"))))
   `(highlight ((t (:background "gray80" :foreground "gray10"))))
   `(minibuffer-prompt ((t (:background "gray10" :foreground "gray80"))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground "gray60"))))
   `(font-lock-comment-face ((t (:foreground "gray35"))))
   `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-constant-face ((t (:foreground "LemonChiffon3"))))
   `(font-lock-doc-face ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-function-name-face ((t (:foreground "gray80" :weight bold))))
   `(font-lock-keyword-face ((t (:foreground "gray80" :weight bold))))
   `(font-lock-negation-char-face ((t nil)))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground "gray75" :slant italic))))
   `(font-lock-type-face ((t (:foreground "gray80" :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground "wheat3"))))
   `(font-lock-warning-face ((t :foreground "DarkOrange")))

   ;; buttons & links
   `(button ((t (:background "gray15" :foreground "gray60" :box (:style released-button)))))
   `(link ((t (:foreground "gray60" :underline t))))
   `(link-visited ((t (:foreground "gray35" :underline t))))
   `(fringe ((t (:background "gray10"))))

   ;; modeline
   `(mode-line ((t (:background "black" :foreground "gray70" :box (:line-width 4 :color "black")))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-inactive ((t (:background "gray15" :foreground "gray50" :box (:line-width 4 :color "gray15")))))
   `(anzu-mode-line ((t (:foreground "firebrick"))))
   ;; sml
   `(sml/col-number ((t (:foreground "gray30"))))
   `(sml/numbers-separator ((t (:foreground "gray30"))))
   `(sml/line-number ((t (:foreground "gray70" :weight normal))))
   `(sml/filename ((t (:foreground "gray70" :weight bold))))
   `(sml/folder ((t (:foreground "gray40"))))
   `(sml/position-percentage ((t (:foreground "gray40" :weight normal))))
   `(sml/prefix ((t (:foreground "gray40" :weight normal))))
   `(sml/client ((t (:foreground "blue" :weight normal))))
   `(sml/minor-modes ((t (:foreground "gray40"))))
   `(sml/modes ((t (:foreground "gray60" :weight bold))))
   `(sml/mule-info ((t (:foreground "gray60"))))
   `(sml/read-only ((t (:foreground "gold3"))))
   `(sml/process ((t (:foreground "gray90"))))
   `(sml/git ((t (:foreground "ForestGreen" :weight normal))))
   `(sml/vc-edited ((t (:foreground "firebrick" :weight normal))))
   `(sml/modified ((t (:foreground "firebrick" :weight bold))))


   `(isearch ((t (:foreground "gray90" :background "gray40" :weight bold))))
   `(lazy-highlight ((t (:foreground "gray90" :background "gray25"))))

   `(show-paren-match ((t (:foreground "black" :background "gray60"))))
   `(show-paren-mismatch ((t (:foreground "black" :background "firebrick"))))

   `(linum ((t (:foreground "gray40" :weight light :height 0.9))))

   `(header-line ((t (:foreground "gray70" :background "gray20"))))

   `(message-header-name ((t (:foreground "gray50" :weight bold))))
   `(message-header-subject ((t (:foreground "gray70"))))
   `(message-header-other ((t (:foreground "gray40"))))

   `(vertical-border ((t (:foreground "black"))))

   `(error ((t (:foreground "firebrick"))))

   `(flycheck-error ((t (:underline (:color "firebrick" :style wave)))))
   `(flycheck-warning ((t (:underline (:color "DarkOrange" :style wave)))))

   `(whitespace-trailing ((t (:background "firebrick "))))

   ;; diredp
   `(diredp-dir-heading ((t (:foreground "gray70" :background "gray20" :weight normal))))
   `(diredp-file-name ((t (:foreground "gray60"))))
   `(diredp-ignored-file-name ((t (:foreground "gray30"))))
   `(diredp-dir-name ((t (:foreground "gray80" :weight bold))))
   `(diredp-file-suffix ((t (:foreground "gray40"))))
   `(diredp-compressed-file-suffix ((t (:foreground "gray30"))))
   `(diredp-date-time ((t (:foreground "gray40"))))
   `(diredp-number ((t (:foreground "gray50"))))
   `(diredp-read-priv ((t (:foreground "gray70"))))
   `(diredp-write-priv ((t (:foreground "gray70"))))
   `(diredp-exec-priv ((t (:foreground "gray70"))))
   `(diredp-dir-priv ((t (:foreground "ForestGreen"))))
   `(diredp-no-priv ((t (:foreground "gray40"))))
   `(diredp-symlink ((t (:foreground "gray70" :background "gray30"))))
   `(diredp-flag-mark-line ((t (:foreground "black" :background "gray60"))))
   `(diredp-flag-mark ((t (:foreground "DarkOrange"))))
   `(diredp-deletion-file-name ((t (:foreground "black" :background "gray60"))))
   `(diredp-deletion ((t (:foreground "firebrick" :weight bold))))

   ;; helm
   `(helm-source-header ((t (:foreground "gray70" :background "gray20"))))
   `(helm-header ((t (:foreground "gray40" :background "gray10"))))
   `(helm-selection ((t (:foreground "gray90" :background "gray30" :weight bold))))
   `(helm-candidate-number ((t (:foreground "black" :background "gray30"))))
   `(helm-match ((t (:foreground "wheat3" :background nil :weight bold))))
   `(helm-M-x-key ((t (:foreground "wheat3" :underline t))))
   `(helm-buffer-directory ((t (:foreground "gray80" :weight bold))))
   `(helm-buffer-size ((t (:foreground "gray30"))))
   `(helm-buffer-process ((t (:foreground "gray35"))))
   `(helm-ff-directory ((t (:foreground "gray80" :weight bold))))
   `(helm-ff-symlink ((t (:foreground "firebrick"))))
   `(helm-visible-mark ((t (:foreground "black" :background "gray60"))))

   ;; helm swoop
   `(helm-swoop-target-line-face ((t (:foreground "gray90" :background "gray30" :weight bold))))
   `(helm-swoop-target-word-face ((t (:foreground "wheat3" :background nil :weight bold))))

   ;; js2-mode
   `(js2-error ((t (:foreground "firebrick"))))
   `(js2-function-param ((t (:foreground "wheat3"))))
   `(js2-warning ((t (:underline (:color "DarkOrange" :style wave)))))

   ;; web mode
   `(web-mode-html-tag-bracket-face ((t (:foreground "wheat3"))))
   `(web-mode-html-tag-face ((t (:foreground "wheat3"))))
   `(web-mode-html-attr-value-face ((t (:foreground "gray50" :slant italic))))
   `(web-mode-html-attr-name-face ((t (:foreground "gray50" :weight bold))))

   ;; comint
   `(comint-highlight-prompt ((t (:foreground "gray80" :background "gray20"))))

   ;; erc
   `(erc-notice-face ((t (:foreground "gray30"))))
   `(erc-nick-msg-face ((t (:foreground "gray70"))))
   `(erc-direct-msg-face ((t (:foreground "wheat3"))))
   `(erc-my-nick-face ((t (:foreground "wheat3" :weight bold))))
   `(erc-input-face ((t (:foreground "LemonChiffon3"))))
   `(erc-default-face ((t (:foreground "gray80"))))
   `(erc-current-nick-face ((t (:foreground "gray90" :weight bold))))
   `(erc-timestamp-face ((t (:foreground "gray40" :weight bold))))
   `(erc-prompt-face ((t (:foreground "gray80" :background "gray20"))))
   `(erc-nick-default-face ((t (:foreground "gray50" :weight bold))))
   `(erc-button ((t (:background "gray15" :foreground "gray60" :box (:style released-button)))))

   ;; elfeed
   `(elfeed-search-date-face ((t (:foreground "gray30"))))
   `(elfeed-search-feed-face ((t (:foreground "gray40"))))
   `(elfeed-search-tag-face ((t (:foreground "wheat3"))))
   `(elfeed-search-title-face ((t (:foreground "gray65"))))
   `(elfeed-log-error-level-face ((t (:foreground "firebrick"))))
   `(elfeed-log-info-level-face ((t (:foreground "gold3"))))
   `(elfeed-search-unread-title-face ((t (:foreground "gray80" :weight bold))))
   )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

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

;; Export
(provide-theme 'paleolithic)

;;; paleolithic-theme.el ends here
