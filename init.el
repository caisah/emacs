;;; init.el - Magic starts here. -*- lexical-binding: t -*-
;;; Code:

(defvar *start-time* (current-time))

;; Define my config dirs
(defconst my-config-dir "~/.emacs.d/config")
(defconst my-elisp-dir "~/.emacs.d/elisp")
(defconst my-themes-dir "~/.emacs.d/themes")

;; Bootstrap Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Add theme
(add-to-list 'custom-theme-load-path my-themes-dir)


;; Add config dir
(add-to-list 'load-path my-elisp-dir)
(add-to-list 'load-path my-config-dir)

;; Load my elisp code
(use-package my-functions)

(use-package no-littering
  :straight t

  :init
  (no-littering-theme-backups)

  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; Set custom file to emacs-custom.el
  (custom-file (expand-file-name (concat my-elisp-dir "/custom.el") user-emacs-directory)))


(use-package emacs
  :config
  ;; Load Theme
  (load-theme 'paleolithic t)

  (if (eql system-type 'darwin)
      (setq-default
       ;; Use coreutils ls installed with homebrew
       insert-directory-program "/opt/homebrew/opt/coreutils/libexec/gnubin/ls"
       ;; use local bash
       shell-file-name "/opt/homebrew/bin/bash"
       ;; Use command as meta
       mac-command-modifier 'meta
       ;; Use option as super
       mac-option-modifier 'super
       ;; Used to disable s-h default shortcut
       mac-pass-command-to-system nil
       ;; set Firefox path to be able to open files
       browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox"
       ;; Use aspell instead of ispell
       ispell-program-name "/opt/homebrew/bin/aspell")

    (setq-default
     ;; Use coreutils ls
     insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls"
     ;; use local bash
     shell-file-name "/usr/local/bin/bash"))

  ;; use UTF-8
  (set-language-environment "UTF-8")

  ;; Load custom file
  (load custom-file)

  ;; Start server - Now we can open any file with emacsclient
  (unless (and (fboundp 'server-running-p)
               (server-running-p))
    (progn
      (server-force-delete)
      (server-start)))

  ;; Enable default disabled stuff
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Use y and n as confirmations
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Global common keys
  (keymap-global-set "C-x K" 'my-kill-other-buffer)
  (keymap-global-set "C-x C-k" 'my-kill-all-buffers)
  (keymap-global-set "C-\\" 'my-delete-to-previous-line)
  (keymap-global-set "C-S-d" 'delete-region)
  (keymap-global-set "C-x M-w" 'my-copy-buffer-file-name)
  (keymap-global-set "C-x i" 'ibuffer)
  (global-unset-key (kbd "C-S-K"))
  (keymap-global-set "C-S-K" 'kill-whole-line)
  (keymap-global-set "M-i"  'my-consult-line)

  (keymap-global-set "C-<tab>" 'indent-relative)

  (keymap-global-set "C-c C-f" 'treesit-fold-toggle)
  (keymap-global-set "s-u" 'revert-buffer)

  ;; Emacs
  (keymap-global-set "C-h C-s" 'elisp-index-search)

  ;; Widow resizing
  (keymap-global-set "C-S-<left>" 'shrink-window-horizontally)
  (keymap-global-set "C-S-<right>" 'enlarge-window-horizontally)
  (keymap-global-set "C-S-<down>" 'shrink-window)
  (keymap-global-set "C-S-<up>" 'enlarge-window)

  ;; Window navigation
  (keymap-global-set "C-S-o" 'previous-window-any-frame)

  (keymap-global-set "<f1>"
                     '(lambda () (interactive) (switch-to-buffer "*Messages*")
                        (end-of-buffer)))

  (keymap-global-set "C-S-s" 'query-replace-regexp)

  ;; Global Modes:
  ;; Use word-wrapping for continuation lines
  (global-visual-line-mode 1)
  ;; Show matching parens
  (show-paren-mode 1)
  ;; Disable electric indent mode
  (electric-indent-mode -1)
  ;; Revert buffer when the file changes on disk
  (global-auto-revert-mode 1)
  ;; Delete selected text when starting to type
  (delete-selection-mode 1)
  ;; Save state between sessions
  (desktop-save-mode t)
  ;; Prettify symbols
  (global-prettify-symbols-mode 1)

  ;; Visual Stuff
  ;; Disable menu bar mode
  (menu-bar-mode -1)
  ;; Disable toolbars
  (tool-bar-mode -1)
  ;; Disable scroll
  (set-scroll-bar-mode nil)
  ;; Fringes
  (set-fringe-mode '(8 . 0))
  ;; Show size of file
  (size-indication-mode t)
  ;; Show column number
  (column-number-mode t)
  ;; prefer utf-8
  (prefer-coding-system 'utf-8)

  :hook
  ;; clean white spaces before saving
  (before-save . whitespace-cleanup)
  ;; display startup time
  (window-setup  . my-show-startup-time)
  ;; show word correcting on programming modes
  (prog-mode . flyspell-prog-mode)

  :custom
  ;; mainly for *scratch*
  (initial-major-mode 'fundamental-mode)
  ;; Set gc limit
  (gc-cons-threshold (* 40 1024 1024))
  ;; Increase the amount of data which Emacs reads from the process
  (read-process-output-max (* 3 1024 1024))
  ;; Make buffer names unique
  (uniquify-buffer-name-style 'post-forward)
  ;; Don't create lock files
  (create-lockfiles nil)
  ;; Use rg
  (grep-program (executable-find "rg"))
  ;; Calendar
  (calendar-latitude 46.7667)
  (calendar-longitude 23.5833)
  ;; Consider all themes safe
  (custom-safe-themes t)
  ;; don't ask to save when compiling
  (compilation-ask-about-save nil)
  ;; don't save abbrevs
  (save-abbrevs nil)
  ;; don't insert tabs on indent
  (indent-tabs-mode nil)
  ;; start emacs fullscreen and maximized
  (initial-frame-alist (quote ((fullscreen . maximized))))
  ;; don't use angle brackets
  (uniquify-buffer-name-style 'post-forward)
  ;; don't put anything is *scratch*
  (initial-scratch-message "")
  ;; highlight trailing spaces & tabs
  (whitespace-style '(face trailing indentation::tabs))
  ;; highlight if more than 180 chars on line
  (whitespace-line-column 180)
  ;; set language for time to Eng
  (system-time-locale "C")
  ;; Always save desktop
  (desktop-save t)
  ;; Move to help buffer when opened
  (help-window-select t)
  ;; Enable recursive minibuffers
  (enable-recursive-minibuffers t)
  ;; Ignore case check on completion
  (completion-ignore-case t)
  ;; Open files with default browser
  (browse-url-browser-function 'browse-url-default-browser)
  ;; Use treesit modes
  (major-mode-remap-alist
   '((sh-mode . bash-ts-mode)
     (css-mode . css-ts-mode)
     (js-mode . js-ts-mode)
     (json-mode . json-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (yaml-mode . yaml-ts-mode)
     (html-mode . html-ts-mode))))


(use-package exec-path-from-shell
  :straight t

  :custom
  ;; add node to the path
  (exec-path-from-shell-copy-env "NVM_DIR")
  ;; add bash config
  (exec-path-from-shell-variables '("PATH" "MANPATH" "BASH_ENV"))
  ;; Don't log bash profile warning
  (exec-path-from-shell-check-startup-files nil)

  :config
  ;; Set exec-path as $PATH
  (exec-path-from-shell-initialize))


(use-package yasnippet
  :straight t

  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets")))


(use-package re-builder
  :custom
  (reb-re-syntax 'string))


(use-package markdown-mode
  :straight t

  :defer t

  :mode
  ("\\.text\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  ("\\.md\\'" . markdown-mode))




(use-package recentf
  :config
  (recentf-mode 1)

  :custom
  ;; Keep max 25 items
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25))


(use-package eldoc
  :config
  (global-eldoc-mode)

  :custom
  (eldoc-idle-delay 0.75))


(use-package yaml-mode
  :straight t

  :mode
  ("\\.yml$" . yaml-mode))


(use-package dotenv-mode
  :straight t

  :defer t

  :mode
  ("\\.env\\..*\\'" . dotenv-mode))


(use-package editorconfig
  :straight t)


(use-package flycheck
  :straight t

  :defer t

  :hook
  (prog-mode . (lambda ()
                 (when (my-current-buffer-too-big-p)
                   (flycheck-mode -1)))))


(use-package rainbow-delimiters
  :straight t)


(use-package expand-region
  :straight t

  :bind
  ("s-I" . er/contract-region)
  ("s-i" . er/expand-region))


(use-package wgrep
  :straight t)


(use-package page-break-lines
  :straight t

  :config
  (global-page-break-lines-mode t))


(use-package dired
  :defer t

  :custom
  (dired-use-ls-dired t)
  ;; Show directories first
  (dired-listing-switches "-lA --group-directories-first")
  ;; Don't ask for confirmation on recursion when copying
  (dired-recursive-copies 'always)
  ;; Don't ask for confirmation on recursion when deleting
  (dired-recursive-deletes 'always)
  ;; Easily copy file to the other buffer
  (dired-dwim-target t)
  ;; Move deleted stuff to trash
  (delete-by-moving-to-trash t)
  ;; Revert buffer on change
  (dired-auto-revert-buffer t)

  ;; Refresh dired when file changes
  :hook ((dired-mode . auto-revert-mode)
         ;; Omit uninteresting files in Dired including .. and .
         (dired-mode . dired-omit-mode))

  :bind (:map dired-mode-map
              ("^" . dired-go-up-dir)
              ("k" . dired-subtree-remove)
              ("i" . dired-subtree-insert)
              ("C-M-u" . dired-subtree-up)
              ("C-M-n" . dired-subtree-next-sibling)
              ("C-M-p" . dired-subtree-previous-sibling)))


(use-package dired-aux
  :after dired

  :config
  ;; Use unzip for .zip files
  (add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))
  ;; Use tar for .tar.xz files
  (add-to-list 'dired-compress-file-suffixes '("\.tar\.xz" ".tar" "tar xf %i")))


(use-package dired+
  :straight t

  :after dired)


(use-package async
  :straight t

  :after dired

  :config
  (dired-async-mode 1))

(use-package dired-quick-sort
  :straight t

  :after dired

  :config
  (dired-quick-sort-setup))


(use-package dired-subtree
  :straight t

  :after dired)


(use-package ibuffer
  :custom
  ;; Set modified char to 'm'
  (ibuffer-modified-char 109)
  ;; Set marked char to '*'
  (ibuffer-marked-char 42)
  ;; Set read only char to 'r'
  (ibuffer-read-only-char 114)
  ;; Sort by major mode
  (ibuffer-default-sorting-mode 'major-mode)
  ;; Customize colors for buffer names
  (ibuffer-fontification-alist
   '((10 (and buffer-file-name
              (buffer-modified-p (get-file-buffer buffer-file-name))) ibuffer-modified-face)
     (20 buffer-read-only ibuffer-read-only-face)
     (30 (string-match "^\\*" (buffer-name)) ibuffer-emacs-buffer-face)
     (35 (derived-mode-p 'dired-mode) font-lock-function-name-face)
     (40 (and (boundp 'emacs-lock-mode) emacs-lock-mode) ibuffer-locked-buffer)))

  :hook
  (ibuffer . (lambda () (toggle-truncate-lines 1))))

(use-package projectile
  :straight t

  :custom
  ;; Use elisp
  (projectile-indexing-method 'alien)
  ;; When switching a project switch to a dir
  (projectile-switch-project-action #'projectile-dired)

  :bind (:map projectile-mode-map
              ("s-h" . projectile-command-map)
              ("s-h g" . consult-ripgrep))

  :hook (prog-mode . projectile-mode))


(use-package hippie-exp
  :custom
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol))
  :config
  (keymap-global-set "M-/" 'hippie-expand))


(use-package epa
  :custom
  ;; can export to text
  (epa-armor t))


(use-package company
  :straight t

  :custom
  (company-auto-update-doc t)

  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              :map  company-mode-map
              ("C-o" . company-capf)))


(use-package company-capf
  :after company)


(use-package company-quickhelp
  :after company

  :straight t

  :config
  ;; show tooltip popup on key press
  (setq company-quickhelp-delay nil)

  :bind (:map company-active-map
              ("C-j" . company-quickhelp-manual-begin)))


(use-package magit
  :straight t

  :config
  (keymap-global-set "C-x m" 'magit-status)
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)

  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (transient-default-level 5)

  :hook (magit-mode . (lambda ()
                        "Make left fringe 20 pixels."
                        (setq left-fringe-width 20
                              right-fringe-width 0))))

(straight-use-package '(vertico :files (:defaults "extensions/*")
                                :includes (vertico-buffer
                                           vertico-directory
                                           vertico-flat
                                           vertico-indexed
                                           vertico-mouse
                                           vertico-quick
                                           vertico-repeat
                                           vertico-reverse
                                           vertico-suspend
                                           vertico-unobtrusive)))

(use-package vertico
  :config
  (vertico-mode 1)

  :custom
  (vertico-cycle t))


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))


;; Configure directory extension.
(use-package vertico-directory
  :after vertico

  :straight nil

  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))

  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


(use-package vertico-multiform
  :after vertico

  :straight nil

  :init
  (vertico-multiform-mode)

  :bind (:map vertico-map
              ("M-J" . vertico-quick-jump)
              ("M-I" . vertico-indexed-mode)))


(use-package marginalia
  :straight t

  :after vertico

  :init
  (marginalia-mode)

  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))


(use-package orderless
  :straight t

  :after vertico

  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


(use-package embark
  :straight t

  :after vertico

  :bind
  (("C-," . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'

   :map embark-collect-mode-map
   ("m" . embark-select))

  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command))


(use-package consult
  :straight t

  :after vertico

  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g d" . consult-flymake)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )


(use-package embark-consult
  :straight t

  :after consult

  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package consult-flycheck
  :straight t

  :after consult)


(use-package doc-view
  :custom
  (doc-view-continuous t))


(use-package ace-window
  :straight t

  :bind
  ("s-o" . ace-window)
  ("C-c M-o" . ace-swap-window)
  ("C-c s-o" . ace-delete-window)

  :custom
  ;; enable deleting, swapping, splitting
  (aw-dispatch-always t)
  ;; keep keys on the home row
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


(use-package smart-mode-line
  :straight t

  :custom
  ;; don't ask for a confirmation when loading sml theme
  (sml/no-confirm-load-theme t)
  ;; use default theme (it's customized in theme anyway)
  (sml/theme 'automatic)
  ;; max 15 chars
  (sml/mode-width 15)
  ;; don't show these modes in mode-line
  (rm-whitelist (mapconcat
                 'identity
                 '("Flymake" "FlyC") "\\|"))

  :config
  (sml/setup))


(use-package eww
  :custom
  (eww-search-prefix "https://duckduckgo.com/lite?q=")
  (shr-color-visible-luminance-min 77)

  :bind
  (:map eww-mode-map
        ("n" . shr-next-link)
        ("p" . shr-previous-link)))


(use-package org
  :custom
  (org-src-fontify-natively t)

  :hook
  (org-mode . flyspell-mode)
  (org-mode . turn-off-smartparens-mode))


(use-package multiple-cursors
  :straight t

  :bind
  (:map global-map
        ("C-S-c C-S-c" . mc/edit-lines)
        ("C->" . mc/mark-next-like-this)
        ("C-<" . mc/mark-previous-like-this)
        ("C-c C-<" . mc/mark-all-like-this)))


(use-package tramp
  :custom
  (tramp-default-method "ssh")
  (tramp-verbose 9)
  (tramp-backup-directory-alist backup-directory-alist)

  :config
  ;; easily sudo with /sudo:root@remote-host:<path-to-file>
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '((regexp-quote (system-name)) nil nil)))


(use-package erc
  :init
  ;; Try to load ercpass file
  (let ((ercpass "~/.ercpass"))
    (if (file-exists-p ercpass)
        (load ercpass)
      (progn
        (setq freenode-pass ""
              snoonet-pass "")
        (message "My init :: warn :: Could not load .ercpass"))))

  :custom
  (erc-nickserv-identify-mode 'both)
  (erc-nickserv-passwords
   `((freenode (("caisah" . ,freenode-pass)))
     (Snoonet (("caisah" . ,snoonet-pass))))
   erc-prompt-for-nickserv-password nil))


(use-package eglot
  :straight nil

  :config
  (add-to-list 'eglot-server-programs '(css-mode . ("vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '((web-mode html-mode) . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode) . my-ts-server-program))
  (add-to-list 'eglot-server-programs '((bash-ts-mode) . ("bash-language-server" "start")))
  (add-to-list 'eglot-server-programs '((json-mode) . ("vscode-json-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '((elixir-ts-mode) . ("/opt/homebrew/Cellar/elixir-ls/0.20.0/libexec/language_server.sh")))
  (add-to-list 'eglot-server-programs '((astro-ts-mode) . ("astro-ls" "--stdio" :initializationOptions
                                                           (:typescript (:tsdk "./node_modules/typescript/lib")))))

  :custom
  (eglot-events-buffer-size 0))


;; (use-package flycheck-eglot
;;   :straight t

;;   :defer t)


(use-package avy
  :straight t

  :config
  (avy-setup-default)
  (keymap-global-set "M-o" 'avy-goto-char)

  :custom
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package treesit-auto
  :straight t

  :custom
  (treesit-auto-install 'prompt)

  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package smartparens
  :straight t

  :init
  (smartparens-global-mode t)

  :bind
  (:map smartparens-mode-map
        ("C-c ("  . wrap-wth-parens)
        ("C-c ["  . wrap-with-brackets)
        ("C-c {"  . wrap-with-braces)
        ("C-c '"  . wrap-with-single-quotes)
        ("C-c \"" . wrap-with-double-quotes)
        ("C-c _"  . wrap-with-underscores)
        ("C-`"  . wrap-with-back-quotes)))

(use-package my-deno
  :defer t)

(use-package apheleia
  :straight t
  :defer t)

(use-package typescript-ts-mode
  :defer t

  :mode
  ("\\.jsx\\'" . tsx-ts-mode)
  ("\\.tsx\\'" . tsx-ts-mode)
  ("\\.mjs\\'" . typescript-ts-mode)

  :config
  (require 'my-deno)

  :custom
  (prettify-symbols-alist '(("function" . "ƒ")))


  :bind
  (:map typescript-ts-base-mode-map
        ("C-c C-d d" . eldoc-doc-buffer)
        ("C-c C-d q" . my-quit-eldoc-buffer)
        ("C-c C-r" . 'my-deno-run-repl)
        ("C-c C-e" . 'my-deno-send-region-to-repl)
        ("C-c C-b" . 'my-deno-send-buffer-to-repl)
        ("C-c C-c" . 'my-deno-reset-repl))

  :hook
  (typescript-ts-base-mode . my-typescript-general-hook))

(use-package js
  :mode
  ("\\.cjs\\'" . js-ts-mode)
  ("\\.js\\'" . js-ts-mode)

  :config
  (require 'my-deno)

  :custom
  (js-indent-level 2)

  :bind
  (:map js-ts-mode-map
        ("C-c C-d d" . eldoc-doc-buffer)
        ("C-c C-d q" . my-quit-eldoc-buffer)
        ("C-c C-r" . 'my-deno-run-repl)
        ("C-c C-e" . 'my-deno-send-region-to-repl)
        ("C-c C-b" . 'my-deno-send-buffer-to-repl)
        ("C-c C-c" . 'my-deno-reset-repl))

  :hook
  ((js-ts-mode . my-prog-modes)
   (js-ts-mode . my-use-eslint-from-node-modules)))


(use-package json-mode
  :straight t

  :bind
  (:map json-mode-map
        ("C-c C-b" . json-mode-beautify))

  :hook
  (json-mode . my-prog-modes))


(use-package esh-mode
  :config
  (keymap-global-set "C-c s" 'my-shell-here)

  :hook (eshell-mode . (lambda ()
                         ;; disable line mode
                         (visual-line-mode nil)
                         ;; show full width lines in shell mode
                         (toggle-truncate-lines 1)
                         ;; start company
                         (company-mode 1))))

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")

  :custom
  (treesit-fold-line-count-show t)

  :config
  (global-treesit-fold-mode))


(use-package nxml-mode
  :hook
  (nxml-mode . sgml-mode))

(use-package web-mode
  :straight t

  :mode
  ("\\.html?\\'" . web-mode)
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.[gj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.hbs?\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.jst\\'" . web-mode)

  :custom
  (web-mode-markup-indent-offset 2)

  :hook
  (web-mode . my-prog-modes))


(use-package css-mode
  :custom
  (css-indent-offset 2)

  :hook
  (css-base-mode . my-prog-modes))

(use-package html-mode
  :hook
  (html-ts-mode . my-prog-modes))


(use-package sh-script
  :hook
  (bash-ts-mode . my-prog-modes))


(use-package elisp-mode
  :hook
  (emacs-lisp-mode . my-prog-modes)
  (emacs-lisp-mode . rainbow-delimiters-mode))


(use-package drag-stuff
  :straight t

  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package elisp-slime-nav
  :after elisp-mode

  :straight t)


(use-package terraform-mode
  :straight t)

(use-package astro-ts-mode
  :straight t

  :mode
  ("\\.astro\\'" . astro-ts-mode)

  :hook
  (astro-ts-mode . my-prog-modes))

(use-package heex-ts-mode
  :straight t)

(use-package elixir-ts-mode
  :hook
  (elixir-ts-mode . my-prog-modes))

(use-package undo-tree
  :straight t

  :init
  (global-undo-tree-mode))

(use-package special-mode
  :bind
  (:map special-mode-map
        (("RET" . my-open-special-markdown-link-at-point))))

(use-package which-key
  :init
  (which-key-mode 1))


(use-package man
  :demand t

  :config
  (Man-init-defvars)

  :custom
  (Man-sed-command "gsed"))

;;; init.el ends here
