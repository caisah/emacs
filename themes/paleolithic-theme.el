;;; paleolithic-theme.el --- Dark Gray Theme

;; Author: Vlad Piersec <vlad.piersec@gmail.com>
;; Keywords: theme, themes, gray, grey, dark
;; Version: 1.0

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
(let ((font-height (if (eql system-type 'darwin) 160 128)))
  (custom-theme-set-faces
   'paleolithic
   `(default ((t (:family "Ubuntu Mono" :height ,font-height :background "gray10" :foreground "gray80"))))
   `(cursor ((t (:background "chartreuse3" :foreground "black"))))
   `(region ((t (:background "gray20" :foreground "gray90"))))
   `(secondary-selection ((t (:background "gray25"))))
   `(highlight ((t (:foreground "wheat3" :background nil :weight bold))))
   `(hl-line ((t (:foreground "gray80" :background "gray20" :weight bold))))
   `(minibuffer-prompt ((t (:foreground "gray55" :background "gray8" :height 0.9 :box (:line-width 4 :color "gray8")))))

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

   ;; common
   `(match ((t (:foreground "goldenRod3" :background "gray25" :weight bold))))
   `(button ((t (:background "gray15" :foreground "gray60" :box (:style released-button)))))
   `(link ((t (:foreground "gray60" :underline t))))
   `(link-visited ((t (:foreground "gray35" :underline t))))
   `(fringe ((t (:background "gray10"))))

   ;; highlighting
   `(hi-yellow ((t (:background "LightGoldenrod" :foreground "gray10"))))

   ;; modeline
   `(mode-line ((t (:background "black" :foreground "gray70" :box (:line-width 4 :color "black")))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-inactive ((t (:background "gray15" :foreground "gray50" :box (:line-width 4 :color "gray15")))))
   `(anzu-mode-line ((t (:foreground "firebrick"))))
   `(anzu-replace-to ((t (:foreground "gray90" :background "gray20" :weight bold))))

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

   ;; ace-window
   `(aw-mode-line-face ((t (:foreground "gray60" :height 0.6))))
   `(aw-leading-char-face ((t (:background "gray10" :foreground "firebrick" :weight bold :height 1.2))))

   `(isearch ((t (:inherit (match)))))
   `(lazy-highlight ((t (:foreground "gray90" :background "gray25"))))

   `(show-paren-match ((t (:foreground "black" :background "gray60"))))
   `(show-paren-mismatch ((t (:foreground "black" :background "firebrick"))))

   `(linum ((t (:foreground "gray40" :background "gray10" :weight light :height 0.9))))
   `(line-number ((t (:inherit linum))))
   `(line-number-current-line ((t (:inherit linum))))

   `(header-line ((t (:foreground "gray70" :background "gray20"))))

   `(message-header-name ((t (:foreground "gray50" :weight bold))))
   `(message-header-subject ((t (:foreground "gray70"))))
   `(message-header-other ((t (:foreground "gray40"))))

   `(vertical-border ((t (:foreground "black"))))

   `(error ((t (:foreground "firebrick"))))

   ;; flycheck
   `(flycheck-error ((t (:underline (:color "firebrick" :style wave)))))
   `(flycheck-warning ((t (:underline (:color "DarkOrange" :style wave)))))
   `(flycheck-error-list-filename ((t (:foreground "wheat3" :weight bold))))
   `(flycheck-error-list-highlight ((t (:background "gray30"))))
   `(flycheck-error-list-column-number ((t (:foreground "gray60"))))
   `(flycheck-error-list-line-number ((t (:foreground "gray60"))))

   ;; whitespace
   `(whitespace-trailing ((t (:background "firebrick"))))
   `(whitespace-line ((t (:background "gray15" :foreground "gray60"))))

   `(custom-variable-tag ((t (:foreground "gray60" :weight bold))))
   `(custom-state ((t (:foreground "wheat3"))))

   ;; eshell
   `(eshell-prompt ((t (:foreground "ForestGreen" :background "black" :weight bold))))
   `(eshell-ls-directory ((t (:foreground "wheat3" :weight bold))))
   `(eshell-ls-symlink ((t (:inherit link))))
   `(eshell-ls-executable ((t (:foreground "LimeGreen"))))
   `(eshell-ls-readonly ((t (:foreground "gray50" :background "black"))))

   ;; org

   `(org-table ((t (:foreground "gray60"))))
   `(org-document-info-keyword ((t (:foreground "gray50"))))
   `(org-document-title ((t (:foreground "LemonChiffon3"))))
   `(org-meta-line ((t (:foreground "gray50"))))
   `(org-date ((t (:foreground "LemonChiffon3" :underline t :height 0.9))))

   `(org-todo ((t (:foreground "gray75" :weight bold :underline "firebrick"))))
   `(org-done ((t (:foreground "gray75" :weight bold :underline "LimeGreen"))))

   `(outline-1 ((t (:foreground "wheat3" :weight bold))))
   `(outline-2 ((t (:foreground "gray80" :weight bold))))
   `(outline-3 ((t (:foreground "gray70" :weight bold))))
   `(outline-4 ((t (:foreground "gray65" :weight bold))))
   `(outline-5 ((t (:foreground "gray65" :weight bold))))
   `(outline-6 ((t (:foreground "gray65" :weight bold))))
   `(outline-7 ((t (:foreground "gray65" :weight bold))))
   `(outline-8 ((t (:foreground "gray65" :weight bold))))

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
   `(diredp-rare-priv ((t (:foreground "gray70" :background "gray30"))))
   `(diredp-flag-mark-line ((t (:foreground "black" :background "gray60"))))
   `(diredp-flag-mark ((t (:foreground "DarkOrange"))))
   `(diredp-deletion-file-name ((t (:foreground "black" :background "gray60"))))
   `(diredp-deletion ((t (:foreground "firebrick" :weight bold))))

   ;; ivy
   `(ivy-action ((t (:foreground "goldenrod3" :background nil :weight bold))))
   `(ivy-current-match ((t (:background "gray25"))))
   `(ivy-grep-info ((t (:foreground "gray40" :height 0.9))))
   `(ivy-grep-line-number ((t (:height 0.9))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground "wheat3" :background nil :weight bold))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground "wheat3" :background nil :weight bold))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground "goldenrod3" :background nil :weight bold))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground "goldenrod3" :background nil :weight bold))))
   `(ivy-match-required-face ((t (:foreground "IndianRed2"))))
   `(ivy-modified-buffer ((t (:foreground "IndianRed2"))))
   `(ivy-modified-outside-buffer ((t (:foreground "IndianRed2"))))
   `(ivy-subdir ((t (:foreground "gray85" :weight bold))))
   `(ivy-remote ((t (:foreground "ForestGreen"))))
   `(ivy-virtual ((t (:foreground "gray50"))))

   ;; swiper
   `(swiper-line-face ((t (:background "gray25"))))
   `(swiper-match-face-1 ((t (:foreground "gray80" :background nil :weight bold))))
   `(swiper-match-face-2 ((t (:foreground "wheat3" :background nil :weight bold))))
   `(swiper-match-face-3 ((t (:foreground "wheat3" :background nil :weight bold))))
   `(swiper-match-face-4 ((t (:foreground "wheat3" :background nil :weight bold))))

   ;; avy
   `(avy-lead-face ((t (:foreground "LemonChiffon3" :background "gray20" :height 0.9))))
   `(avy-lead-face-0 ((t (:foreground "LemonChiffon3" :background "gray20" :height 0.9))))
   `(avy-lead-face-1 ((t (:foreground "LemonChiffon3" :background "gray20" :height 0.9))))
   `(avy-lead-face-2 ((t (:foreground "LemonChiffon3" :background "gray20" :height 0.9))))

   ;; hydra
   `(hydra-face-pink ((t (:foreground "wheat3" :weight bold))))
   `(hydra-face-red ((t (:foreground "IndianRed2" :weight bold))))
   `(hydra-face-blue ((t (:foreground "goldenrod3" :weight bold))))

   ;; helm
   `(helm-source-header ((t (:foreground "gray70" :background "gray15"))))
   `(helm-header ((t (:foreground "gray40" :background "gray10"))))
   `(helm-selection ((t (:background "gray20"))))
   `(helm-grep-match ((t (:foreground "wheat3" :background nil :weight bold))))
   `(helm-candidate-number ((t (:foreground "black" :background "gray30"))))
   `(helm-match ((t (:foreground "wheat3" :background nil :weight bold))))
   `(helm-M-x-key ((t (:foreground "wheat3" :underline t))))
   `(helm-moccur-buffer ((t (:foreground "gray60" :underline t))))
   `(helm-grep-lineno ((t (:foreground "gray50"))))
   `(helm-buffer-directory ((t (:foreground "gray80" :weight bold))))
   `(helm-buffer-size ((t (:foreground "gray35"))))
   `(helm-buffer-process ((t (:foreground "gray35"))))
   `(helm-buffer-not-saved ((t (:foreground "IndianRed2"))))
   `(helm-buffer-modified ((t (:foreground "IndianRed2"))))
   `(helm-ff-directory ((t (:foreground "gray80" :weight bold))))
   `(helm-ff-dotted-directory ((t (:foreground "gray40" :background "gray10"))))
   `(helm-ff-prefix ((t (:foreground "gray70" :background "gray30"))))
   `(helm-ff-symlink ((t (:foreground "firebrick"))))
   `(helm-visible-mark ((t (:foreground "black" :background "gray60"))))
   `(helm-separator ((t (:foreground "gray40"))))

   ;; helm swoop
   `(helm-swoop-target-line-face ((t (:foreground "gray90" :background "gray30" :weight bold))))
   `(helm-swoop-target-word-face ((t (:foreground "wheat3" :background nil :weight bold))))
   `(helm-swoop-target-line-block-face ((t (:inherit (helm-swoop-target-line-face)))))
   `(helm-swoop-line-number-face ((t (:foreground "red"))))

   `(helm-xref-file-name ((t (:inherit 'helm-moccur-buffer))))
   `(helm-xref-line-number ((t (:inherit 'helm-grep-lineno))))


   ;; js2-mode
   `(js2-error ((t (:foreground "firebrick"))))
   `(js2-function-param ((t (:foreground "wheat3"))))
   `(js2-warning ((t (:underline (:color "DarkOrange" :style wave)))))
   `(js2-jsdoc-tag ((t (:foreground "gray30"))))
   `(js2-jsdoc-type ((t (:foreground "gray40"))))
   `(js2-jsdoc-value ((t (:foreground "gray52"))))

   ;; typescript
   `(typescript-jsdoc-tag ((t (:inherit (js2-jsdoc-tag)))))
   `(typescript-jsdoc-type ((t (:inherit (js2-jsdoc-type)))))
   `(typescript-jsdoc-value ((t (:inherit (js2-jsdoc-value)))))


   ;; web mode
   `(web-mode-html-tag-bracket-face ((t (:foreground "wheat3"))))
   `(web-mode-html-tag-face ((t (:foreground "wheat3"))))
   `(web-mode-html-attr-value-face ((t (:foreground "gray50" :slant italic))))
   `(web-mode-html-attr-name-face ((t (:foreground "gray50" :weight bold))))

   ;; comint
   `(comint-highlight-prompt ((t (:foreground "gray80" :background "gray20"))))
   `(comint-highlight-input ((t (:foreground "gray85" :weight bold))))

   ;; hydra
   `(hydra-face-blue ((t (:foreground "DarkRed" :weight bold))))

   ;; markdown
   `(markdown-header-delimiter-face ((t (:foreground "wheat3" :weight normal))))
   `(markdown-language-keyword-face ((t (:foreground "grey35" :weight normal))))
   `(markdown-code-face ((t (:foreground "grey55" :weight normal :height 0.95 :family "Ubuntu Mono"))))
   ;; erc
   `(erc-notice-face ((t (:foreground "gray25"))))
   `(erc-nick-msg-face ((t (:foreground "gray70"))))
   `(erc-direct-msg-face ((t (:foreground "wheat3"))))
   `(erc-my-nick-face ((t (:foreground "wheat3" :weight bold))))
   `(erc-input-face ((t (:foreground "LemonChiffon3"))))
   `(erc-default-face ((t (:foreground "gray75"))))
   `(erc-current-nick-face ((t (:foreground "gray90" :weight bold))))
   `(erc-timestamp-face ((t (:foreground "gray40" :weight bold))))
   `(erc-prompt-face ((t (:foreground "gray80" :background "gray20"))))
   `(erc-nick-default-face ((t (:foreground "gray40" :weight bold))))
   `(erc-button ((t (:background "gray15" :foreground "gray60" :box (:style released-button)))))
   `(erc-error-face ((t (:foreground "firebrick"))))

   ;; elfeed
   `(elfeed-search-date-face ((t (:foreground "gray30"))))
   `(elfeed-search-feed-face ((t (:foreground "gray40"))))
   `(elfeed-search-tag-face ((t (:foreground "wheat3"))))
   `(elfeed-search-title-face ((t (:foreground "gray65"))))
   `(elfeed-log-error-level-face ((t (:foreground "firebrick"))))
   `(elfeed-log-info-level-face ((t (:foreground "gold3"))))
   `(elfeed-search-unread-title-face ((t (:foreground "gray80" :weight bold))))

   ;; ediff
   `(ediff-odd-diff-A ((t (:background "gray5" :foreground "gray60"))))
   `(ediff-odd-diff-B ((t (:background "gray5" :foreground "gray60"))))
   `(ediff-odd-diff-C ((t (:background "gray5" :foreground "gray60"))))
   `(ediff-even-diff-A ((t (:background "gray5" :foreground "gray60"))))
   `(ediff-even-diff-B ((t (:background "gray5" :foreground "gray60"))))
   `(ediff-even-diff-C ((t (:background "gray5" :foreground "gray60"))))

   `(ediff-current-diff-A ((t (:background "gray20" :foreground "gray90" :weight bold))))
   `(ediff-current-diff-B ((t (:background "gray20" :foreground "gray90" :weight bold))))
   `(ediff-current-diff-C ((t (:background "gray20" :foreground "gray90" :weight bold))))

   `(ediff-fine-diff-A ((t (:background "gray20" :foreground "firebrick"))))
   `(ediff-fine-diff-B ((t (:background "gray20" :foreground "DarkOrange"))))
   `(ediff-fine-diff-C ((t (:background "gray20" :foreground "LimeGreen"))))


   ;; magit
   `(magit-log-author ((t (:foreground "gray50"))))
   `(magit-log-date ((t (:foreground "gray35"))))

   `(magit-section-heading ((t (:foreground "gray70" :weight bold))))
   `(magit-section-highlight ((t (:foreground "gray80" :background "gray20"))))
   `(magit-section-heading-selection ((t (:foreground "gray95"))))

   `(magit-branch-current ((t (:foreground "wheat3" :box (:line-width 1)))))
   `(magit-branch-local ((t (:foreground "grey70" :box (:line-width 1)))))
   `(magit-branch-remote ((t (:foreground "gray55" :box (:line-width 1)))))

   `(magit-diff-added ((t (:foreground "ForestGreen" :background "gray15"))))
   `(magit-diff-added-highlight ((t (:foreground "LimeGreen" :background "black"))))

   `(magit-diff-removed ((t (:foreground "DarkRed" :background "gray15"))))
   `(magit-diff-removed-highlight ((t (:foreground "firebrick" :background "black"))))

   `(magit-diff-hunk-heading ((t (:foreground "gray70" :background "gray15"))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground "gray70" :background "gray20"))))

   `(magit-diff-context ((t (:foreground "gray50" :background "gray15"))))
   `(magit-diff-context-highlight ((t (:foreground "gray75" :background "gray5"))))

   `(magit-diff-file-heading ((t (:foreground "wheat3" :background "gray15"))))
   `(magit-diff-file-heading-highlight ((t (:foreground "wheat3"  :weight bold :background "gray20"))))

   `(magit-reflog-commit ((t (:foreground "gray60"))))
   `(magit-reflog-merge ((t (:foreground "LimeGreen"))))
   `(magit-reflog-amend ((t (:foreground "gray65"))))
   `(magit-reflog-checkout ((t (:foreground "ForestGreen"))))
   `(magit-reflog-cherry-pick ((t (:foreground "ForestGreen"))))
   `(magit-reflog-rebase ((t (:foreground "wheat3"))))
   `(magit-reflog-remote ((t (:foreground "LemonChiffon3"))))
   `(magit-reflog-other ((t (:foreground "gray50"))))
   `(magit-reflog-reset ((t (:foreground "firebrick"))))

   `(epa-validity-high ((t (:foreground "gray90" :weight bold))))
   `(epa-field-name ((t (:foreground "gray90" :weight bold))))
   `(epa-field-body ((t (:foreground "gray70" :slant italic))))

   `(reb-match-0 ((t (:background "gray40"))))
   `(reb-match-1 ((t (:background "gray30"))))

   `(company-tooltip ((t (:background "black" :foreground "gray70"))))
   `(company-tooltip-selection ((t (:foreground "gray90" :background "gray30" :weight bold))))
   `(company-tooltip-common ((t (:foreground "wheat3" :weight bold))))
   `(company-tooltip-annotation ((t (:foreground "wheat3"))))
   `(company-scrollbar-fg ((t (:background "gray50"))))
   `(company-scrollbar-bg ((t (:background "gray30"))))
   `(company-preview-common ((t (:foreground "gray70" :background "gray20"))))

   `(tooltip ((t (:foreground "gray80" :background "gray20"))))

   ;; Merlin
   `(merlin-compilation-error-face ((t (:inherit 'flycheck-error))))

   ;; LSP
   `(lsp-face-highlight-write ((t (:background "gray35" :foreground "gray85"))))

   ;; discover key mode
   `(makey-key-mode-button-face ((t (:foreground "wheat3" :underline t))))

   `(Info-quoted ((t (:background "gray20" :box (:line-width 2 :color "grey20") :height 1.0 :width condensed))))
   ))

(custom-theme-set-variables
 'paleolithic
 '(ansi-color-names-vector ["gray35" "gray75" "gray75" "wheat" "gray75" "gray65" "gray75" "gray75" "gray75" "gray75"])
 '(ansi-color-map (ansi-color-make-color-map))
 '(ibuffer-marked-face 'diredp-flag-mark)
 '(ibuffer-deletion-face 'diredp-deletion-file-name))

;; Export
(provide-theme 'paleolithic)
;;; paleolithic-theme.el ends here
