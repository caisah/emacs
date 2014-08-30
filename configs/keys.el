		  ;; Non -programming languages- keys:



;; Abbrevs

;; Global Abbrevs
(define-abbrev-table 'global-abbrev-table '(
;; Greek small letters
	("8al" "α")
	("8be" "β")
	("8ga" "γ")
	("8de" "δ")
	("8ep" "ε")
	("8ze" "ζ")
	("8et" "η")
	("8th" "θ")
	("8io" "ι")
	("8ka" "κ")
	("8la" "λ")
	("8mu" "μ")
	("8nu" "ν")
	("8xi" "ξ")
	("8oi" "ο") ;; omicron
	("8pi" "π")
	("8ro" "ρ")
	("8si" "σ")
	("8ta" "τ")
	("8up" "υ")
	("8ph" "φ")
	("8ch" "χ")
	("8ps" "ψ")
	("8oe" "ω") ;; omega
 
	;; Greek capital letters
	("8Al" "Α")
	("8Be" "Β")
	("8Ga" "Γ")
	("8De" "Δ")
	("8Ep" "Ε")
	("8Ze" "Ζ")
	("8Et" "Η")
	("8Th" "Θ")
	("8Io" "Ι")
	("8Ka" "Κ")
	("8La" "Λ")
	("8Mu" "Μ")
	("8Nu" "Ν")
	("8Xi" "Ξ")
	("8Oi" "Ο") ;; Omicron
	("8Pi" "Π")
	("8Ro" "Ρ")
	("8Si" "Σ")
	("8Ta" "Τ")
	("8Up" "Υ")
	("8Ph" "Φ")
	("8Ch" "Χ")
	("8Ps" "Ψ")
	("8Oe" "Ω") ;; Omega					    
))

;; Global Keys


;; Search
(global-set-key (kbd "C-r") 'query-replace)

;; Fullscreen
(global-set-key (kbd "C-x 5 f") 'toggle-fullscreen)

;; Dired+ keys
(define-key ctl-x-map   "d" 'diredp-dired-files)
(define-key ctl-x-4-map "d" 'diredp-dired-files-other-window)

; Ibuffer instead of Buffer menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Kill ring
(global-set-key (kbd "M-y") 'browse-kill-ring)

; Delete
(global-set-key (kbd "C-S-d") 'delete-region)

;; Smart-compile
(global-set-key (kbd "<f9>") 'smart-compile)

;; Hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)

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


;; Expand region
(global-set-key (kbd "C-`") 'er/expand-region)

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-:") 'mc/mark-pop)

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; This is the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "<C-tab>") 'indent-relative)

;; Toggle hiding
(global-set-key (kbd "C-c C-f") 'hs-toggle-hiding)


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

;; Keys for html
(defun html-keys ()
  "Keys used in HTML"
  (local-set-key (kbd "C-,") 'sgml-tag)
  (local-set-key (kbd "C-o") 'open-file-in-browser)
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (local-set-key (kbd "C-j") 'newline-and-indent)
  (local-set-key (kbd "C-;") 'emmet-expand-line)
  (local-set-key (kbd "M-n") 'sp-html-next-tag)
  (local-set-key (kbd "M-p") 'sp-html-previous-tag)
  (local-set-key (kbd "C-M-n") 'web-mode-element-next)
  (local-set-key (kbd "C-M-p") 'web-mode-element-previous)
  (local-set-key (kbd "C-M-d") 'web-mode-element-child)
  (local-set-key (kbd "C-M-u") 'web-mode-element-parent)
  (local-set-key (kbd "C-M-a") 'web-mode-element-beginning)
  (local-set-key (kbd "C-M-e") 'web-mode-element-end)
  (local-set-key (kbd "C-M-f") 'web-mode-attribute-next)
  (local-set-key (kbd "C-M-b") 'web-mode-attribute-previous)
  (local-set-key (kbd "C-M-k") 'web-mode-element-kill)
  (local-set-key (kbd "C-S-i") 'web-mode-element-content-select)
  (local-set-key (kbd "C-S-c") 'web-mode-element-clone)
  (local-set-key (kbd "<C-M-backspace>") 'backward-kill-element)
  )

;; Quit on Q
;; (define-key help-mode-map (kbd "q") 'kill-this-buffer)
;; (define-key apropos-mode-map (kbd "q") 'kill-this-buffer)
;; (define-key special-mode-map (kbd "q") 'kill-this-buffer)


;; Macros

(global-set-key (kbd "C-\\")
		(lambda () (interactive)
		  (delete-horizontal-space)
		  (backward-delete-char 1)
		  (delete-horizontal-space)
		  (insert-char 32)))

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

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
