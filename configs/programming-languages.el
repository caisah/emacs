			  ;; Scheme/Racket:


;; Abbrevs:
(define-abbrev-table 'scheme-mode-abbrev-table '(
 ("lam" "lambda")
 ("def" "define")
 ("dis" "display")
 ("che" "check-equal?")))

(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'pretty-symbols-mode)
(add-hook 'scheme-mode-hook 'smartparens-strict-mode)
(add-hook 'scheme-mode-hook 'abbrev-mode)
(add-hook 'geiser-mode-hook 'geiser-local-keys)
(add-hook 'find-file-hook 'disable-geiser-and-add-keys)

;; The binary of the interpreter
(setq scheme-program-name "mit-scheme-x86-64")

(defun scheme-local-keys ()
  (local-set-key (kbd "C-x C-r") 'scheme-send-region))

(defun geiser-local-keys ()
  (local-set-key (kbd "C-c C-q") 'geiser-restart-repl)
  (local-set-key (kbd "C-x C-r") 'geiser-eval-region)
  (define-key geiser-mode-map (kbd "C-.") 'imenu-anywhere)
  (bind-smartparens-locally))

(defun disable-geiser-and-add-keys ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.scm\\'" buffer-file-name))
    (geiser-mode -1)
    (scheme-local-keys)))

(require 'quack)


				;; C:


;; Abbrevs:
(define-abbrev-table 'c-mode-abbrev-table '(
  ("inc" "#include")
  ("io" "<stdio.h>")
  ("lib" "<stdlib.h>")
  ("ing" "<string.h>")
  ("def" "#define")
  ("ret" "return")
  ("mai" "int main(void)")
  ("main" "int main(int argc, char *argv[])" )
))

(add-hook 'c-mode-common-hook 'linum-mode)
(add-hook 'c-mode-common-hook 'abbrev-mode)


			    ;; Emacs Lisp


;; Abbrevs:
(define-abbrev-table 'emacs-lisp-mode-abbrev-table '(
  ("lam" "lambda")						     
  ("def" "defun")
  ("mes" "message")
  ("int" "(interactive)")
  ("sav" "(save-excursion)")
))

(defun local-elisp-keys ()
  (bind-smartparens-locally))

;; Enable rainbow delimiters
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook 'pretty-symbols-mode) 
(add-hook 'emacs-lisp-mode-hook 'abbrev-mode)
(add-hook 'emacs-lisp-mode-hook 'local-elisp-keys)


			     ;; Smalltalk


(require 'smalltalk-mode)
(require 'gst-mode)

(push (cons "\\.star\\'"
            (catch 'archive-mode
              (dolist (mode-assoc auto-mode-alist 'archive-mode)
                (and (string-match (car mode-assoc) "Starfile.zip")
                     (functionp (cdr mode-assoc))
                     (throw 'archive-mode (cdr mode-assoc))))))
      auto-mode-alist)

(push '("\\.st\\'" . smalltalk-mode) auto-mode-alist)

(autoload 'smalltalk-mode "smalltalk-mode" "" t)
(autoload 'gst "gst-mode" "" t)
(add-hook 'smalltalk-mode-hook 'linum-mode)



			    ;; JavaScript

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'pretty-symbol-patterns '(?ƒ lambda "\\<function\\>" (js2-mode)))

(define-abbrev-table 'js2-mode-abbrev-table '(
  ("fun" "function")                                     
  ("ret" "return")
))


(defun skewer-eval-region (beg end)
  "Execute the region as JavaScript code in the attached browsers."
  (interactive "r")
  (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))


;; Skewer
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'js2-mode-hook 'pretty-symbols-mode)
(add-hook 'js2-mode-hook 'linum-mode)
(add-hook 'js2-mode-hook 'abbrev-mode)
(add-hook 'js2-mode-hook 'js-keys)
(add-hook 'js2-mode-hook 'smartparens-strict-mode)
(add-hook 'js2-mode-hook 'bind-smartparens-locally)
(add-hook 'js2-mode-hook 'hs-minor-mode)


			       ;; HTML

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(setq web-mode-markup-indent-offset 2)

;; 
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'flycheck-mode)
(add-hook 'web-mode-hook 'html-keys)
(add-hook 'web-mode-hook 'skewer-html-mode)
(add-hook 'web-mode-hook 'skewer-css-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)


(defun open-file-in-browser()
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url filename)))

(defun backward-kill-element ()
  (interactive) 
  (backward-word)
  (web-mode-element-kill))


			      ;; Haskell

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'linum-mode)

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))

(eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "haskell-mode"
  '(progn
    (define-key haskell-mode-map (kbd "C-x C-d") nil)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c M-.") nil)
    (define-key haskell-mode-map (kbd "C-c C-d") nil)))

(setq haskell-interactive-popup-errors nil)
