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

;; (require 'quack)


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
