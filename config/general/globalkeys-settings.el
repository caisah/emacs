;; Global Keys 
(global-set-key (kbd "C-S-s") 'query-replace)

(global-set-key (kbd "C-S-d") 'delete-region)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x m") 'magit-status)

(global-set-key (kbd "C-`") 'er/expand-region)

(global-set-key (kbd "<C-tab>") 'indent-relative) 

(global-set-key (kbd "C-c C-f") 'hs-toggle-hiding)

(global-set-key (kbd "C-h C-s") 'elisp-index-search)

(defun kill-other-buffer ()
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window 1))
(global-set-key (kbd "C-x K") 'kill-other-buffer)

;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-S-f") 'helm-projectile)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-.") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-/") 'helm-find)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
(global-set-key (kbd "C-x C-S-f") 'helm-projectile)
(global-set-key (kbd "C-x g") 'helm-do-grep-recursive)
(global-set-key (kbd "C-x C-b") '(lambda ()
                                   (interactive)
                                   (helm-resume t)))


;; Smart-compile
(global-set-key (kbd "<f9>") 'smart-compile)

;; Widow resizing
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; SmartParens
;; Wrap the next expression in a pair of parens
(defun my-wrap-with-round-paren (&optional arg)
  (interactive "p")
  (sp-select-next-thing-exchange arg)
  (execute-kbd-macro (kbd "(")))
(define-key sp-keymap (kbd "C-(") 'my-wrap-with-round-paren)


(defun bind-smartparens-locally ()
  (local-set-key (kbd "C-M-n") 'sp-next-sexp)
  (local-set-key (kbd "C-M-p") 'sp-previous-sexp)
  (local-set-key (kbd "C-M-e") 'sp-end-of-sexp)
  (local-set-key (kbd "C-M-a") 'sp-beginning-of-sexp)
  (local-set-key (kbd "C-M-k") 'sp-kill-sexp)
  (local-set-key (kbd "C-M-y") 'sp-backward-unwrap-sexp)
  (local-set-key (kbd "C-M-o") 'sp-unwrap-sexp)
  (local-set-key (kbd "C-}") 'sp-select-next-thing)
  (local-set-key (kbd "C-{") 'sp-select-previous-thing)
  (local-set-key (kbd "C-M-w") 'sp-copy-sexp)
  (local-set-key (kbd "<C-M-backspace>") 'sp-backward-kill-sexp)
  (local-set-key (kbd "C-M-0") 'sp-forward-slurp-sexp)
  (local-set-key (kbd "C-M-9") 'sp-backward-slurp-sexp)
  (local-set-key (kbd "C-M-8") 'sp-forward-barf-sexp)
  (local-set-key (kbd "C-M-7") 'sp-backward-barf-sexp)
  (local-set-key (kbd "C-M-y") 'sp-backward-unwrap-sexp)
  (local-set-key (kbd "C-M-t") 'sp-transpose-sexp))
(add-hook 'smartparens-enabled-hook 'bind-smartparens-locally)

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-:") 'mc/mark-pop)

;; Keys for js-comint/Node.js
(defun node-keys ()
  (interactive)
  (setq skewer-mode nil)
  (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
  (local-set-key (kbd "C-x C-r") 'js-send-region)
  (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
  (local-set-key (kbd "C-c b") 'js-send-buffer)
  (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go))

;; Kesys for js2 mode
(defun js-keys ()
  "Keys used in Javascript"
  (interactive)
  (local-set-key (kbd "C-x C-r") 'skewer-eval-region)
  (local-set-key (kbd "C-c C-f") 'hs-toggle-hiding))

(defun delete-to-previous-line ()
  (interactive)
  (delete-horizontal-space)
  (backward-delete-char 1)
  (delete-horizontal-space)
  (insert-char 32))
(global-set-key (kbd "C-\\") 'delete-to-previous-line)

(defun yank-next (&optional arg)
 (interactive)
  (end-of-line)
  (newline-and-indent)
  (yank arg))
(global-set-key (kbd "C-S-y") 'yank-next)

(defun next-line-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "C-S-j") 'next-line-and-indent)    


(provide 'globalkeys-settings)
