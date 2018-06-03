;;Use-package bootstrap for new install
(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))

(defun minibuffer-increase-threshold ()
  "Small function I stole from somebodys init. Used for entering the minibuffers for autocomplete/fuzzy searching and simply increases the threshold"
  (setq gc-cons-threshold most-positive-fixnum))

(defun minibuffer-normal-threshold ()
  "Another small function i stole. Instead of increasing the gc threshold, it brings it to normal(that is 800 KB)"
  (setq gc-cons-threshold 800000))

(defun open-init-file ()
"Open the init file written in org"
(interactive)
(find-file "~/.emacs.d/newInit.org"))

(defun exit-bracket ()
"Exit out of the brackets and goes to the end of the line"
(interactive)
(evil-normal-state 1)
(evil-append-line 1))

(defun start-external-shell ()
"Start an external shell, whatever the default system shell is"
(interactive)
(start-process "shell-process" nil "xfce4-terminal"))

(defun remove-initial-scratch-buffer ()
"Remove the scratch buffer on startup"
(interactive)
(if (get-buffer "*scratch*")
    (kill-buffer "*scratch*")))

(defun org-enable-math ()
"Enable latex symbol and command auto complete inside of org/markdown buffers. Has to be called manually"
  (interactive)
  (company-mode)
  (add-to-list 'company-backends '(company-math-symbols-unicode company-latex-commands)))

;;Increases threshold to the maximum, helps not slow down fuzzy searches
(add-hook 'minibuffer-setup-hook #'minibuffer-increase-threshold)
;;Returns it to normal afterwards
(add-hook 'minibuffer-exit-hook #'minibuffer-normal-threshold)

;;Remove some of the default tool bars and scroll bars   
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Remove splash screen and startup message
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t)

;;Smooth scrolling
(setq scroll-conservatively 100)

(use-package doom-themes
			 :ensure t)
;;Theme settings
(load-theme 'doom-dracula t)
(setq doom-themes-enable-bold t
	  doom-themes-enable-italic t)

;;Hook is executed after the entire file is loaded. Removes the scratch buffer
;; (add-hook 'after-init-hook 'remove-initial-scratch-buffer)

;;Disable backup files
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;;Disable auto-compression, encryption
(setq auto-compression-mode nil)
(setq auto-encryption-mode nil)

;;Disable tooltips
(setq x-gtk-use-system-tooltips nil)
(setq tooltip-mode nil)

;;Column numbers(turned on only in prog modes)
(add-hook 'prog-mode-hook 'column-number-mode)

;; Relative line numbers, emacs26 >= only
(setq-default display-line-numbers 'relative)

;;Visual Parentheses matching
(show-paren-mode 1)

;;Add parentheses matching
(electric-pair-mode 1)

;;Tab width
(setq-default tab-width 4)

;;Require y or n instead of full yes or no for destructive commands
(fset 'yes-or-no-p 'y-or-n-p)

;;Use bash for shell. Will use custom .bashrc
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;;Set the initial buffer to org todo list
(setf initial-buffer-choice (lambda () (find-file "~/Org/Todo.org")))


;; Stop at 80th column
;; (add-hook 'prog-mode-hook '(setq fill-column 80))
;; (add-hook 'text-mode-hook '(setq fill-column 80))

;; Visual indicators for wrap lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;;Wrap lines so they do not go past screen edge
(global-visual-line-mode 1)

;;Evil leader setup
(use-package evil-leader
      :ensure t
      :config
      (global-evil-leader-mode)
      (evil-leader/set-leader "<SPC>"))
;;-------------------------- Evil leader bindings 
;;Window navigation
(evil-leader/set-key "wj" 'evil-window-down
					 "wh" 'evil-window-left
					 "wk" 'evil-window-down
					 "wl" 'evil-window-right
					 ;;Quick switch to next window
					 "ww" 'evil-window-next) 

;;Kill window or window and buffer
(evil-leader/set-key "wd" 'evil-window-delete
					 "wk" 'kill-buffer-and-window) 

;;Create new vertical/horizontal windows
(evil-leader/set-key "nv" 'evil-window-vsplit
					 "nh" 'evil-window-split) ;New horizontal window
;;Balance windows
(evil-leader/set-key "wb" 'balance-windows)

;;Cycle through available buffers
(evil-leader/set-key "wq" 'evil-prev-buffer
					 "we" 'evil-next-buffer)

;;Show all buffers available 
(evil-leader/set-key "ws" 'helm-buffers-list)

;;Create a new buffer with given input
(evil-leader/set-key "nb" 'switch-to-buffer)

;;Open up external shell(async process)
(evil-leader/set-key "ss" 'start-external-shell)
;;Open up internal emacs shell
(evil-leader/set-key "si" 'ansi-term)


;;Create a new file from a buffer. Does not save the file, use :w for that
(evil-leader/set-key "fw" 'write-file)

;;Open init file
(evil-leader/set-key "/" 'open-init-file)

;;Open up a buffer describing all key bindings
(evil-leader/set-key "K" 'describe-bindings)

;;Double tap on leader(spacebar) will bring up command execution(M-x)
(evil-leader/set-key "<SPC>" 'helm-M-x)

;;Open up package manager
(evil-leader/set-key "." 'package-list-packages)

;;----------------------- Mode specific bindings for leader
;;-----Elisp bindings
;;Eval the entire buffer
(evil-leader/set-key-for-mode 'emacs-lisp-mode "," 'eval-buffer)

;;-----Markdown bindings
;;Headings
(evil-leader/set-key-for-mode 'markdown-mode "d1" 'markdown-insert-header-atx-1)
(evil-leader/set-key-for-mode 'markdown-mode "d2" 'markdown-insert-header-atx-2)
(evil-leader/set-key-for-mode 'markdown-mode "d3" 'markdown-insert-header-atx-3)
(evil-leader/set-key-for-mode 'markdown-mode "d4" 'markdown-insert-header-atx-4)
(evil-leader/set-key-for-mode 'markdown-mode "d5" 'markdown-insert-header-atx-5)
(evil-leader/set-key-for-mode 'markdown-mode "d6" 'markdown-insert-header-atx-6)
;;Insert/format text
(evil-leader/set-key-for-mode 'markdown-mode "dd" #'(lambda ()
							      (interactive)
							      (evil-append-line 1)
							      (markdown-insert-list-item 1)
							 ))
(evil-leader/set-key-for-mode 'markdown-mode "ds" 'markdown-insert-bold)
(evil-leader/set-key-for-mode 'markdown-mode "di" 'markdown-insert-italic)
;;Table inserts
(evil-leader/set-key-for-mode 'markdown-mode "dr" 'markdown-table-insert-row)
(evil-leader/set-key-for-mode 'markdown-mode "dc" 'markdown-table-insert-column)
;;Horizontal line
(evil-leader/set-key-for-mode 'markdown-mode "dh" 'markdown-insert-hr)
;;Demote/Promote elements
(evil-leader/set-key-for-mode 'markdown-mode "dp" 'markdown-demote)
(evil-leader/set-key-for-mode 'markdown-mode "de" 'markdown-promote)
;;Open up pandoc hydra
(evil-leader/set-key-for-mode 'markdown-mode "d[" 'pandoc-main-hydra/body)
;;Preview output in emacs browser
(evil-leader/set-key-for-mode 'markdown-mode "do" 'markdown-live-preview-mode)

;;------Treemacs bindings
;;Toggle on/off
(evil-leader/set-key "ff" 'treemacs)
;;Different ways of opening a file
(evil-leader/set-key-for-mode 'treemacs-mode "h" 'treemacs-visit-node-vertical-split)
(evil-leader/set-key-for-mode 'treemacs-mode "v" 'treemacs-visit-node-horizontal-split)
(evil-leader/set-key-for-mode 'treemacs-mode "o" 'treemacs-visit-node-no-split)
;;Show dotfiles, this is disabled by default
(evil-leader/set-key-for-mode 'treemacs-mode "s" 'treemacs-toggle-show-dotfiles)

;;------Org Mode Bindings
;;Agenda
(evil-leader/set-key-for-mode 'org-mode "da" 'org-agenda)
;;Insert todo heading(inserts new line, inserts heading then enters insert mode)
(evil-leader/set-key-for-mode 'org-mode "dd" #'(lambda ()
												 (interactive)
												 (evil-append-line 1)
												 (org-insert-todo-heading nil)
												 (evil-append-line 1)))
;;Insert a table
(evil-leader/set-key-for-mode 'org-mode "dt" 'org-table-create-or-convert-from-region)
;;Open the link at point
(evil-leader/set-key-for-mode 'org-mode "do" 'org-open-at-point)
;;Insert a link
(evil-leader/set-key-for-mode 'org-mode "dl" 'org-insert-link)
;;Schedule the item
(evil-leader/set-key-for-mode 'org-mode "ds" #'(lambda ()
						 (interactive)
						 (org-schedule 1)
						 (org-cycle )))
;; Set a tag for a todo item
(evil-leader/set-key-for-mode 'org-mode "dm" 'org-ctrl-c-ctrl-c)
;; Insert a deadline for some item(usually todo's)
(evil-leader/set-key-for-mode 'org-mode "di" 'org-deadline)
;;Compilation menu
(evil-leader/set-key-for-mode 'org-mode "dc" 'org-export-dispatch)

(evil-leader/set-key-for-mode 'org-mode "de" 'org-edit-special)

;; Insert different levels of headings
(evil-leader/set-key-for-mode 'org-mode "d1" #'(lambda () (interactive) (insert "* ") (evil-append-line 1)))
(evil-leader/set-key-for-mode 'org-mode "d2" #'(lambda () (interactive) (insert "** ") (evil-append-line 1)))
(evil-leader/set-key-for-mode 'org-mode "d3" #'(lambda () (interactive) (insert "*** ") (evil-append-line 1)))
(evil-leader/set-key-for-mode 'org-mode "d4" #'(lambda () (interactive) (insert "**** ") (evil-append-line 1)))
(evil-leader/set-key-for-mode 'org-mode "d5" #'(lambda () (interactive) (insert "***** ") (evil-append-line 1)))
(evil-leader/set-key-for-mode 'org-mode "d6" #'(lambda () (interactive) (insert "****** ") (evil-append-line 1)))

;;------Lisp Mode Bindings
;;Start Slime
(evil-leader/set-key-for-mode 'lisp-mode "ds" 'slime)
;;Compile and load entire file
(evil-leader/set-key-for-mode 'lisp-mode "dc" 'slime-compile-and-load-file)
;;Eval one function
(evil-leader/set-key-for-mode 'lisp-mode "df" 'slime-compile-defun)
;;Switch to output buffer
(evil-leader/set-key-for-mode 'lisp-mode "dr" 'slime-switch-to-output-buffer)

;;Evil Nerd commenter
(evil-leader/set-key "cl" 'evilnc-comment-or-uncomment-lines)
(evil-leader/set-key "cp" 'evilnc-comment-or-uncomment-paragraphs)

;;Shell
(evil-leader/set-key-for-mode 'ansi-term "dd" '(term-send-raw))

(evil-leader/set-key "r" 'evil-use-register)

;;Some emacs commands
(evil-leader/set-key "cc" 'kill-ring-save
					 "cv" 'yank)

;;Enable evil mode everywhere. The initialization is deferred to let evil leader load first
(use-package evil
  :ensure t
  :init
  :config
  (setq evil-want-C-u-scroll t)
  (evil-mode 1)
  :after (evil-leader))

;;Resizing of windows. (C is the control key)
(define-key evil-normal-state-map (kbd "<C-left>") 'evil-window-decrease-width)
(define-key evil-normal-state-map (kbd "<C-right>") 'evil-window-increase-width)
(define-key evil-normal-state-map (kbd "<C-up>") 'evil-window-increase-height)
(define-key evil-normal-state-map (kbd "<C-down>") 'evil-window-decrease-height)

;;For package manager
(define-key package-menu-mode-map (kbd "j") 'next-line)
(define-key package-menu-mode-map (kbd "k") 'previous-line)

;;Enter console in Insert state
(evil-set-initial-state 'ansi-term 'insert)

;;Visual lines
(define-key evil-normal-state-map (kbd "C-j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "C-k") 'evil-previous-visual-line)

;;Exit out of brackets while in insert mode
(define-key evil-insert-state-map (kbd "C-a") 'exit-bracket)

;;Treemacs mode mappings

;; Go to parent node of current sub-tree
(evil-define-key 'normal treemacs-mode-map (kbd "h") 'treemacs-goto-parent-node)


;;Colorfull cursor depending on state
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("white" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("white" bar))
(setq evil-replace-state-cursor '("green" bar))
(setq evil-operator-state-cursor '("red" hollow))

;;Make escape quit anything
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map [escape] 'keyboard-quit)

(use-package helm
:ensure t
:config
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-d") 'helm-buffer-run-kill-persistent)
)

(use-package projectile
:ensure t
:defer t)

(use-package helm-projectile
:ensure t
:after projectile
:config
(evil-leader/set-key "ps" 'helm-projectile-switch-project)
(evil-leader/set-key "pf" 'helm-projectile-find-file)
(evil-leader/set-key "pd" 'helm-projectile-find-dir))

;;Org mode todo states
(setq org-todo-keywords '((sequence "TODO" "MAYBE" "WAITING" "|" "DONE" "CANCELLED")))

;;Make it so agenda opens horizontally
(setq split-height-threshold 40)
(setq split-width-threshold nil)
(setq org-agenda-window-setup 'reorganize-frame)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-window-frame-fractions '(0.7 . 0.8))

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-log-done 'time)
  (setq org-deadline-warning-days 14)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-span (quote 4))
  (setq org-agenda-start-day "-1d")
  (setq org-agenda-remove-tags t)
  (setq org-tag-alist '(("@school" . ?s) ("@home" . ?h) ("@errand" . ?e) ("@goal" . ?g)))
  :config
  (setq org-file-apps
      '((auto-mode . emacs)
      ("\\.pdf\\'" . "zathura %s")
      ("\\.epub\\'" . "zathura %s")))
  )


  (use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
          '((:name "Today" :todo "TODO")
          (:name "School" :todo ("TEST" "ADMIN" "ASSIGNMENT"))
          (:name "Daily" :todo "HABIT")
          (:name "Maybe" :todo "MAYBE"))))

(use-package company-math
  :ensure t
  :defer t)

(use-package treemacs
:ensure t
:defer t 
:config
(setq treemacs-show-hidden-files nil))

(use-package treemacs-evil
:ensure t
:after (treemacs))

(use-package markdown-mode
:ensure t
:defer t
:init (setq markdown-command "pandoc")
:config (setq markdown-enable-math t)
(setq markdown-live-preview-mode t)
:commands (markdown-mode gfm-mode)
:mode ("\\.md\\'" . markdown-mode)
		("README\\.md\\'" . gfm-mode)
		("\\.markdown\\'" . markdown-mode)
	:hook (add-hook 'markdown-mode-hook 'pandoc-mode))

(use-package evil-nerd-commenter
:ensure t
:defer t)

(use-package pdf-tools
:ensure t
:mode ("\\.pdf\\'" . pdf-tools-install)
:defer t)

(use-package pomodoro
:ensure t
:defer t
:config
(pomodoro-add-to-mode-line))

(use-package pandoc-mode
:ensure t
:hook (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
:after (markdown-mode))

;;Snippets manager
(use-package yasnippet
			:ensure t)

;; Actualy snippets 
(use-package yasnippet-snippets
:ensure t)

;;Used to async linting for many languages
(use-package flycheck
  :ensure t
  :defer t
  :config
  (flycheck-pos-tip-mode))

(use-package flycheck-pos-tip
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer t
  :init
  (evil-leader/set-key "g" 'magit))

(use-package magithub
  :ensure t
  :after (magit))

;;fuzzy matching on completions
;;Slows it down too much but I will keep it for the future
(use-package company-flx
  :ensure t
  :defer t)

;;Display tooltips for functions. Only activated in emacs lisp mode
(use-package company-quickhelp
  :ensure t
  :defer t)

;;frontend for completions
(use-package company
            :ensure t
            :config
            (setq company-idle-delay 0.1)
            (setq company-minimum-prefix-length 1)
            (setq company-tooltip-align-annotations t)
            (setq company-show-numbers t)

            ;;Keybindings for company selections
            (define-key company-active-map (kbd "M-n") nil)
            (define-key company-active-map (kbd "M-p") nil)
            (define-key company-active-map (kbd "C-j") 'company-select-next)
            (define-key company-active-map (kbd "C-k") 'company-select-previous)
            (define-key company-active-map [tab] 'company-complete-common-or-cycle)
            (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle))


;;Keeps a file containing the most used completions
(use-package company-statistics
:ensure t
:after (company)
:defer t)

;;Activate company mode in lisp mode
(use-package slime-company
:ensure t
:defer t)

;;Set up slime
(use-package slime
:ensure t
:mode ("\\.cl\\'" . lisp-mode) 
:config
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy slime-company)))

(use-package common-lisp-snippets
:ensure t
:defer t)

;;Elisp hook for auto complete
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;;Hook for common lisp. Starts up the REPL
(add-hook 'lisp-mode-hook '(lambda ()
				(company-mode)
				(slime)
				(require 'common-lisp-snippets)
				(company-statistics-mode)
				(yas-minor-mode)))

(setq racer-cmd "~/.cargo/bin/racer")
;; (setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
(use-package rust-mode
:ensure t
:mode ("\\.rs\\'" . rust-mode)
:config
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'racer-mode-hook 'eldoc-mode)
(add-hook 'racer-mode-hook 'company-mode)
(setq rust-format-on-save t)
(company-statistics-mode))

(use-package cargo
:ensure t
:defer t)

(use-package racer
:ensure t
:defer t)

;; (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
;;   (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
;;   (add-to-list 'exec-path my-cabal-path))
;; (custom-set-variables '(haskell-tags-on-save t))

;; (use-package haskell-mode
;;   :ensure t
;;   :mode "\\.hs\\'"
;;   :init
;;   (add-hook 'haskell-mode-hook 'hindent-mode)
;;   (autoload 'ghc-init "ghc" nil t)
;;   (autoload 'ghc-debug "ghc" nil t)
;;   (add-hook 'haskell-mode-hook (lambda () (ghc-init)
;; 				 ))
;;   :config
;;   (add-to-list 'company-backends 'company-ghc)
;;   (add-hook 'haskell-mode-hook 'company-mode)
;;   (custom-set-variables '(company-ghc-show-info t)
;; 			'(haskell-process-suggest-remove-import-lines t)
;; 			'(haskell-process-auto-import-loaded-modules t)
;; 			'(haskell-process-log t)
;; 			'(haskell-process-type 'cabal-repl)))

;; Due to issues with installing ghc-mod on manjaro, this will replace it

(use-package haskell-snippets
  :ensure t
  :defer t)

(use-package intero
  :ensure t
  :defer t
  :init (add-hook 'haskell-mode-hook 'intero-mode)
  :config
  (yas-minor-mode)
  (flycheck-mode))

;;Default emacs python mode, set up a hook for it to enable elpy
(use-package python
  :ensure t
  :mode ("\\.py" . python-mode)
  :config
  (add-hook 'python-mode-hook 'elpy-mode))

  (use-package py-autopep8
    :ensure t
    :defer t)

  (use-package elpy
    :ensure t
    :defer t
    :config
    ;;Use standard python interpreter to run files
    (setq python-shell-interpreter "python"
          python-shell-interpreter-args "-i")
    ;; use flycheck instead of flymake
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)
    (yas-minor-mode)
    (company-statistics-mode)
    (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(use-package basic-c-compile
  :ensure t
  :defer t)

(use-package company-irony-c-headers
  :ensure t
  :defer t)

(use-package company-irony
            :ensure t
            :config
            (company-mode)
            (add-to-list 'company-backends '(company-irony-c-headers company-irony)))

(use-package irony
            :ensure t
            :config
            (add-hook 'c-mode-hook 'irony-mode)
            (add-hook 'c++-mode-hook 'irony-mode)
            (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(add-hook 'c-mode-hook '(lambda ()
                (company-mode)
                (yas-minor-mode)
                (company-statistics-mode)
                (flycheck-mode)))

(add-hook 'c++-mode-hook '(lambda ()
                (company-mode)
                (yas-minor-mode)
                (company-statistics-mode)
                (flycheck-mode)))

;; (use-package js2-mode
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.js" . js2-mode))

;;   ;; Better imenu
;;   (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

;; (use-package js2-refactor
;;   :ensure t
;;   :init   (add-hook 'js2-mode-hook 'js2-refactor-mode)
;;   :config (js2r-add-keybindings-with-prefix "C-c ."))


;; (use-package xref-js2
;;   :ensure t
;;   :after js2-mode
;;   :config

;;   (add-hook 'js2-mode-hook
;;             (lambda ()
;;               (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

;; (use-package company-tern
;;   :ensure t
;;   :after js2-mode
;;   :config
;;   (require 'company)
;;   (add-to-list 'company-backends 'company-tern)
;;   (add-hook 'js2-mode-hook (lambda ()
;;                              (tern-mode)
;;                              (company-mode))))

(use-package rjsx-mode
:ensure t
:mode ("\\.js\\'" . rjsx-mode))

;; (use-package tide
;;   :ensure t
;;   :defer t
;;   :mode ("\\.js\\'" . tide-mode)
;;   :config
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   (company-mode +1))
;;   
;;

(use-package nasm-mode
:ensure t
:config
(add-hook 'asm-mode-hook 'nasm-mode))

(use-package company-web
:ensure t
:config
(require 'company-web-html))

(use-package web-mode
:ensure t
:config
(require 'company-web-html))
(add-hook 'css-mode-hook (lambda ()
			(company-mode)))
