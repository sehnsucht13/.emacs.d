;;Use-package bootstrap for new install
(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))


;; Always ensure all packages
(setq use-package-always-ensure t)

;; Logging
;; (setq use-package-verbose t)

;; more logging
(setq use-package-compute-statistics t)

(defun minibuffer-increase-threshold ()
  "Small function I stole from somebodys init. Used for entering the minibuffers for autocomplete/fuzzy searching and simply increases the threshold"
  (setq gc-cons-threshold most-positive-fixnum))

(defun minibuffer-normal-threshold ()
  "Another small function i stole. Instead of increasing the gc threshold, it brings it to normal(that is 800 KB)"
  (setq gc-cons-threshold 1000000))

;;Increases threshold to the maximum, helps not slow down fuzzy searches
(add-hook 'minibuffer-setup-hook #'minibuffer-increase-threshold)
;;Returns it to normal afterwards
(add-hook 'minibuffer-exit-hook #'minibuffer-normal-threshold)

;;Disable backup files
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;;Disable auto-compression, encryption
(setq auto-compression-mode nil)
(setq auto-encryption-mode nil)

;;Disable tooltips
(setq x-gtk-use-system-tooltips t)
(setq tooltip-mode t)

;; line break at 80 chars
(setq-default fill-column 80)

;; Wrap at 80 automatically
(if (< emacs-major-version 27)
    (add-hook 'text-mode-hook 'refill-mode)
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  )

;; Relative line numbers 
(setq display-line-numbers-type 'relative)

;; Enable line numbers, emacs26/27  only in programming modes.
(add-hook 'prog-mode-hook (lambda ()
                            ;; Show line numbers in sideline
                            (display-line-numbers-mode)
                            ;; Show column numbers in mode line
                            (column-number-mode)
                            ;; Wrap at 80
                            (turn-on-auto-fill)
                            ;;Visual Parentheses matching
                            (show-paren-mode 1)
                            ))



;;Add parentheses matching
(electric-pair-mode 1)

;;Tab width
(setq-default tab-width 4)

;; Do not use tabs
(setq-default indent-tabs-mode nil)

;;Require y or n instead of full yes or no for destructive commands
(fset 'yes-or-no-p 'y-or-n-p)

;;Use bash for shell. Will use custom .bashrc
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)


;; Visual indicators for wrap lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;;Stop keys being echoed in minibuffer. Messes up which-key
(setq echo-keystrokes 0)

;; Clear image cache sooner to save memory on larger pdfs. Measured in seconds.
(setq image-cache-eviction-delay 15)

;;Always focus help buffers
(setq help-window-select t)

;;Set initial buffer mode to text mode.
(setq initial-major-mode 'emacs-lisp-mode)

(add-hook 'prog-mode-hook (lambda ()
                            (hl-line-mode)
                            (visual-line-mode)
                            ))

(add-hook 'text-mode-hook (lambda ()
                            (hl-line-mode)
                            (visual-line-mode)))

;; Set hack as default font
(set-frame-font "Hack 11" nil t)
(add-to-list 'default-frame-alist '(font . "Hack 11"))

;;Evil leader setup
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))
;;-------------------------- Evil leader bindings 
;;Window navigation
(evil-leader/set-key "j" 'evil-window-down
                     "h" 'evil-window-left
                     "k" 'evil-window-down
                     "l" 'evil-window-right
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

;; Narrow text
(evil-leader/set-key "wn" 'narrow-to-region)

;; Widen text back
(evil-leader/set-key "wi" 'widen)

;;Cycle through available buffers
(evil-leader/set-key "wq" 'evil-prev-buffer
                     "we" 'evil-next-buffer)

;;Show all buffers available 
(evil-leader/set-key "ws" 'helm-buffers-list)

;;Open up external shell(async process)
(evil-leader/set-key "ss" 'start-external-shell)

;;Open up internal emacs shell
(evil-leader/set-key "si" 'ansi-term)

;;Create a new file from a buffer. Does not save the file, use :w for that
(evil-leader/set-key "fw" 'write-file)

;;Open init file
(evil-leader/set-key "/" 'open-init-file)

;;Eval new init file
(evil-leader/set-key "?" 'eval-new-init-file)

;;Double tap on leader(spacebar) will bring up command execution(M-x)
(evil-leader/set-key "<SPC>" 'helm-M-x)

;;Open up package manager
(evil-leader/set-key "." 'package-list-packages)

;;----------------------- Mode specific bindings for leader
;;-----Elisp bindings
;;Eval the entire buffer
(evil-leader/set-key-for-mode 'emacs-lisp-mode "," 'eval-buffer)



;;------Lisp Mode Bindings
;;Start Slime
(evil-leader/set-key-for-mode 'lisp-mode "ds" 'slime)
;;Compile and load entire file
(evil-leader/set-key-for-mode 'lisp-mode "dc" 'slime-compile-and-load-file)
;;Eval one function
(evil-leader/set-key-for-mode 'lisp-mode "df" 'slime-compile-defun)
;;Switch to output buffer
(evil-leader/set-key-for-mode 'lisp-mode "dr" 'slime-switch-to-output-buffer)

;;Shell
(evil-leader/set-key-for-mode 'ansi-term "dd" '(term-send-raw))

;;Show bookmarks list
(evil-leader/set-key "bl" 'helm-filtered-bookmarks)

;;Enable evil mode everywhere. The initialization is deferred to let evil leader load first
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-u-scroll t)
  :after (evil-leader)
  :config
  (evil-mode 1))
;; Make C-u work in normal and visual mode.
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

;;Resizing of windows. (C is the control key)
(define-key evil-normal-state-map (kbd "<C-left>") 'evil-window-decrease-width)
(define-key evil-normal-state-map (kbd "<C-right>") 'evil-window-increase-width)
(define-key evil-normal-state-map (kbd "<C-up>") 'evil-window-increase-height)
(define-key evil-normal-state-map (kbd "<C-down>") 'evil-window-decrease-height)


;;Enter console in Insert state
(evil-set-initial-state 'ansi-term 'insert)

;;Visual lines
(define-key evil-normal-state-map (kbd "M-j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "M-k") 'evil-previous-visual-line)

;;Exit out of brackets while in insert mode
(define-key evil-insert-state-map (kbd "C-a") 'exit-bracket)

;;Colorfull cursor depending on state
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("red" box))
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

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

(use-package hydra
  :ensure t
  )

(use-package transient
  :ensure t
  )

(use-package helm
  :ensure t
  :init
  ;; Enable helm mode
  (helm-mode 1)
  :config
  (setq helm-mode-fuzzy-match t)
  (setq helm-split-window-default-side 'below)
  ;; Basic navigation
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-d") 'helm-buffer-run-kill-persistent)
  (define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)

  ;; Find files in current dir
  (evil-leader/set-key "ff" 'helm-find-files)

  ;; Man pages
  (evil-leader/set-key "fm" 'helm-man-woman)

  ;; Locate some file across the system
  (evil-leader/set-key "fl" 'helm-locate)

  ;; Find function defs
  (evil-leader/set-key "fa" 'helm-apropos)

  ;; Find occurances of some word or regexp
  (evil-leader/set-key "fo" 'helm-occur)

  ;;Resume previous session
  (evil-leader/set-key "fp" 'helm-resume)

  ;; Open dired
  (evil-leader/set-key "fd" 'dired)

  ;; Imenu or semantic, usefull for quick navigation of files
  (evil-leader/set-key "fi" 'helm-semantic-or-imenu)

  ;; View register contents
  (evil-leader/set-key "fr" 'helm-register)

  )

(use-package helm-rg
  :ensure t
  )

(defun open-init-file ()
"Open the init file written in org"
(interactive)
(find-file "~/.emacs.d/newInit.org"))

(defun eval-new-init-file ()
  "Evaluates the init.el file and then closes it. Used to update config after changing anything in org-mode based init file"
  (interactive)
  (eval-buffer (find-file user-init-file))
  (kill-buffer (buffer-name)))

(defun exit-bracket ()
"Exit out of the brackets and go to the end of the line."
(interactive)
(evil-normal-state 1)
(evil-append-line 1))

(defun start-external-shell ()
"Start an external shell, whatever the default system shell is."
(interactive)
(start-process "shell-process" nil "xfce4-terminal"))

(defun make-my-bookmark ()
  "Automatically create a bookmark with the name Current + filename."
  (interactive)
  (bookmark-set (buffer-name)))


(defhydra hydra-manjaro-files ()
  ("h" (helm-find-file-as-root "/etc/hosts") "Hosts File" :exit t)
  ("i" (find-file "~/.config/i3/config") "I3 Config" :exit t)
  ("b" (find-file "~/.bashrc") "Bash Config" :exit t)
  ("f" (find-file "~/.emacs.d/elfeed.org") "Feeds" :exit t))

(defhydra hydra-personal ()
  ("n" (run-elfeed) "News" :exit t)
  ("e" (find-file "~/.emacs.d/newInit.org") "Emacs init" :exit t)
  ("a" (find-file "~/Org/Agenda.org") "Agenda" :exit t)
  ("g" (find-file "~/.gitignore_global") "Global Gitignore" :exit t)
  ("f" (hydra-manjaro-files/body) "Files" :exit t))

;; Open up personal hydra
(evil-leader/set-key "'" 'hydra-personal/body)

(load-file "~/pprojects/helm-org-wiki/helm-org-wiki.el")

(evil-leader/set-key  "ti" 'helm-org-wiki-open-index)
(evil-leader/set-key "tw" 'helm-org-wiki-walk-wiki)
(evil-leader/set-key "tn" 'helm-org-wiki-create-new-article)
(evil-leader/set-key "tb" 'helm-org-wiki-open-reading-list)

(evil-leader/set-key-for-mode 'org-mode "ih" 'helm-org-wiki-haskell-block)
(evil-leader/set-key-for-mode 'org-mode "ija" 'helm-org-wiki-java-block)
(evil-leader/set-key-for-mode 'org-mode "ijs" 'helm-org-wiki-javascript-block)
(evil-leader/set-key-for-mode 'org-mode "ip" 'helm-org-wiki-python-block)
(evil-leader/set-key-for-mode 'org-mode "ic" 'helm-org-wiki-C-block)
(evil-leader/set-key-for-mode 'org-mode "iv" 'helm-org-wiki-C++-block)
(evil-leader/set-key-for-mode 'org-mode "ir" 'helm-org-wiki-rust-block)
(evil-leader/set-key-for-mode 'org-mode "ie" 'helm-org-wiki-emacs-lisp-block)
(evil-leader/set-key-for-mode 'org-mode "ila" 'helm-org-wiki-latex-block)
(evil-leader/set-key-for-mode 'org-mode "ilp" 'helm-org-wiki-lisp-block)
(evil-leader/set-key-for-mode 'org-mode "is" 'helm-org-wiki-sh-block)

(use-package avy
  :ensure t
  :config
  (evil-leader/set-key "as" 'avy-goto-char-2)
  (evil-leader/set-key "al" 'avy-goto-char-in-line))

;; New terminal emulator
(use-package vterm
  :ensure t
  :config
(add-hook 'vterm-mode-hook
          (lambda ()
            (setq-local evil-insert-state-cursor 'box)
            (evil-insert-state)))
(define-key vterm-mode-map [return]                      #'vterm-send-return)

(setq vterm-keymap-exceptions nil)
(evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
(evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
;; (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-libvterm)
;; (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-libvterm-next)
;; (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-libvterm-prev)
(evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
(evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
(evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))


(use-package multi-libvterm
  :load-path "~/.emacs.d/multi-libvterm"
  :config
  (setq multi-libvterm-dedicated-window-height 100)
  (evil-global-set-key 'normal (kbd "C-,") 'multi-libvterm-projectile)
  (evil-global-set-key 'insert (kbd "C-,") 'multi-libvterm-projectile)
  (evil-leader/set-key "sp" 'multi-libvterm-projectile)
  (evil-leader/set-key "so" 'multi-libvterm)
  (evil-leader/set-key "sl" 'multi-libvterm-next)
  (evil-leader/set-key "sh" 'multi-libvterm-prev))

(use-package rainbow-delimiters
  :ensure t)

;;Snippets manager
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; Actual snippets 
(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet))

(use-package projectile
  :ensure t
  :config
  (evil-leader/set-key "pa" 'projectile-discover-projects-in-directory)
  (evil-leader/set-key "pk" 'projectile-kill-buffers)
  (projectile-mode 1)
  (setq projectile-enable-caching t)
  (push ".ccls-cache" projectile-globally-ignored-directories)
  )

(use-package helm-projectile
  :ensure t 
  :after (projectile)
  :config
  (helm-projectile-on)
  (setq helm-projectile-fuzzy-match t)
  ;; Master menu
  (evil-leader/set-key "pp" 'helm-projectile)
  ;; Switches to projects
  (evil-leader/set-key "ps" 'helm-projectile-switch-project)
  ;; Switches to a project buffer
  (evil-leader/set-key "pb" 'helm-projectile-switch-to-buffer)
  ;; Use ripgrep on project
  (evil-leader/set-key "pg" 'helm-projectile-rg)
  ;; Invalidate cache for current project/remove a project
  (evil-leader/set-key "pr" 'helm-projectile-remove-known-project)
  ;; Compile the project
  (evil-leader/set-key "pc" 'helm-projectile-compile-project)
  )

;; Use counsel projectile to find files. Helm-projectile is not optimized to
;; handle large amounts of them
(use-package counsel-projectile
  :ensure t
  :after projectile
  :config
  ;; Finds a file within project
  (evil-leader/set-key "pf" 'counsel-projectile-find-file)
  ;; Finds a directory and opens it within project
  (evil-leader/set-key "pd" 'counsel-projectile-find-dir))

(use-package treemacs
:ensure t
:defer t
:init
;;Toggle on/off
(evil-leader/set-key "ft" 'treemacs)
:config
;;Different ways of opening a file
(evil-leader/set-key-for-mode 'treemacs-mode "h" 'treemacs-visit-node-vertical-split)
(evil-leader/set-key-for-mode 'treemacs-mode "v" 'treemacs-visit-node-horizontal-split)
(evil-leader/set-key-for-mode 'treemacs-mode "o" 'treemacs-visit-node-no-split)
;;Show dotfiles, this is disabled by default
(evil-leader/set-key-for-mode 'treemacs-mode "s" 'treemacs-toggle-show-dotfiles)
(setq treemacs-show-hidden-files nil))

(use-package treemacs-evil
:ensure t
:after (treemacs))

(use-package markdown-mode
:ensure t
:mode ("\\.md\\'" . markdown-mode)
("README\\.md\\'" . gfm-mode)
("\\.markdown\\'" . markdown-mode)
:hook (add-hook 'markdown-mode-hook 'pandoc-mode)
:defer t
:init 
:config
(setq markdown-command "pandoc")
(setq markdown-enable-math t)
(setq markdown-live-preview-mode t)
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
:commands (markdown-mode gfm-mode))

(use-package evil-nerd-commenter
  :ensure t
  :defer t
  :init
  (evil-leader/set-key "cp" 'evilnc-comment-or-uncomment-paragraphs)
  (evil-leader/set-key "cl" 'evilnc-comment-or-uncomment-lines))

;; Stolen from the evil collection
(defun evil-collection-pdf-view-next-line-or-next-page (&optional count)
  "'evil' wrapper include a count argument to `pdf-view-next-line-or-next-page'"
  (interactive "P")
      (if count
          (dotimes (_ count nil)
        (pdf-view-next-page 1))
        (pdf-view-next-line-or-next-page 3)))

(defun evil-collection-pdf-view-previous-line-or-previous-page (&optional count)
  "'evil' wrapper include a count argument to `pdf-view-previous-line-or-previous-page'"
  (interactive "P")
  (if count
      (dotimes (_ count nil)
        (pdf-view-previous-page 1))
    (pdf-view-previous-line-or-previous-page 3)))

(defun evil-collection-pdf-view-goto-page (&optional page)
      "`evil' wrapper around `pdf-view-last-page'."
      (interactive "P")
      (if page
          (pdf-view-goto-page page)
        (pdf-view-last-page)
        (image-eob)))

;; (use-package pdf-tools
;;   :ensure t
;;     :mode ("\\.pdf\\'" . pdf-view-mode)
;;     :config
;;     (pdf-tools-install)
;;     (setq pdf-view-continuous t)
;;     (setq pdf-view-display-size 'fit-width)
;;     (evil-set-initial-state 'pdf-view-mode 'normal)
;;     (evil-define-key 'normal pdf-view-mode-map (kbd "j") 'evil-collection-pdf-view-next-line-or-next-page
;;       (kbd "k") 'evil-collection-pdf-view-previous-line-or-previous-page
;;       (kbd "J") 'pdf-view-next-page
;;       (kbd "K") 'pdf-view-previous-page
;;       (kbd "i") 'pdf-outline
;;       (kbd "q") 'bury-buffer
;;       (kbd "Q") 'kill-current-buffer
;;       (kbd "gg") 'pdf-view-first-page
;;       (kbd "G") 'evil-collection-pdf-view-goto-page))


;; (setq doc-view-continuous t)
;; (evil-set-initial-state 'doc-view-mode 'normal)
;;       (evil-define-key 'normal doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page
;;         (kbd "k") 'doc-view-previous-line-or-previous-page
;;         (kbd "J") 'doc-view-next-page
;;         (kbd "K") 'doc-view-previous-page
;;         (kbd "q") 'bury-buffer
;;         (kbd "Q") 'kill-current-buffer
;;         (kbd "gg") 'doc-view-first-page
;;         (kbd "G") 'doc-view-last-page)

(use-package pandoc-mode
  :ensure t
  :defer t
  :init (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  (evil-leader/set-key "[" (lambda ()
                             (pandoc-@-hydra/pandoc-main-hydra/body-and-exit))))

;;Used to async linting for many languages
(use-package flycheck
  :ensure t
  :defer t
  :hook((prog-mode . flycheck-mode))
  :config
  (evil-leader/set-key "ej" 'flycheck-next-error)
  (evil-leader/set-key "ek" 'flycheck-previous-error))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode)
  )

;; Git interface
(use-package magit
  :ensure t
  :defer t
  :init
  (require 'git-commit)
  (evil-leader/set-key "ms" 'magit-status)
  (evil-leader/set-key "mp" 'magit-push)
  (evil-leader/set-key "mc" 'magit-commit)
  (evil-leader/set-key "md" 'magit-pull))

;; List all todos in repo
(use-package magit-todos
  :ensure t
  :after magit
  :config
  (evil-leader/set-key "mt" 'magit-todos-list))

;;Bindings for the emacs calendar. Used often with deadlines and overall agenda related tasks
(define-key calendar-mode-map "j" 'calendar-forward-day)
(define-key calendar-mode-map "k" 'calendar-backward-day)

;; Control weeks
(define-key calendar-mode-map (kbd "C-j") 'calendar-forward-week)
(define-key calendar-mode-map (kbd "C-k") 'calendar-backward-week)

;; Control months
(define-key calendar-mode-map (kbd "C-h") 'calendar-backward-month)
(define-key calendar-mode-map (kbd "C-l") 'calendar-forward-month)

(defun my-dired-mode-setup ()
  "Runs as a hook when dired mode starts. Disables some features I find annoying"
  (dired-hide-details-mode t)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda ()
                                         (interactive)
                                         (find-alternate-file "..")))
  (evil-define-key 'normal dired-mode-map "H" (lambda ()
                                                (interactive)
                                                (find-alternate-file "..")))
  (evil-define-key 'normal dired-mode-map "L" 'dired-find-alternate-file))

;; Enables normal copy and paste
(use-package dired-ranger
  :ensure t
  :bind (:map dired-mode-map
              ("C" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("P" . dired-ranger-paste)))

;; Provides various customizable filters. Simply avoids writing regexps everytime
(use-package dired-filter
  :after (dired-ranger)
  :ensure t
  :config
  (define-key dired-mode-map (kbd "M-f") dired-filter-map))


;; Run the hook
(add-hook 'dired-mode-hook 'my-dired-mode-setup)

;; Add an auto filter for dotfiles
(add-hook 'dired-mode-hook 'dired-filter-by-dot-files)

(setq dired-recursive-copies (quote always))

(setq dired-recursive-deletes (quote top))

(evil-define-key 'normal dired-mode-map "Q" (lambda ()
                                              (interactive)
                                              (quit-window t)))
(evil-leader/set-key "fh" (lambda ()
                            (interactive)
                            (dired "~/")))

;; Simple shortcuts for my bookmarks
(evil-global-set-key 'normal ",q" (lambda ()
                                    (interactive)
                                    (bookmark-maybe-load-default-file)
                                    (bookmark-jump "Books")))
(evil-global-set-key 'normal ",w" (lambda ()
                                    (interactive)
                                    (bookmark-maybe-load-default-file)
                                    (bookmark-jump "Downloads")))
(evil-global-set-key 'normal ",s" (lambda ()
                                    (interactive)
                                    (bookmark-maybe-load-default-file)
                                    (bookmark-jump "School")))
(evil-global-set-key 'normal ",i" (lambda ()
                                    (interactive)
                                    (bookmark-maybe-load-default-file)
                                    (bookmark-jump "EmacsInit")))

(evil-global-set-key 'normal ",o" (lambda ()
                                    (interactive)
                                    (bookmark-maybe-load-default-file)
                                    (bookmark-jump "OrgFiles")))

(use-package elfeed
  :ensure t
  :defer t
  :config
  (evil-define-key 'normal elfeed-search-mode-map "q" 'elfeed-search-quit-window
    "o" 'elfeed-search-browse-url
    "e" 'run-elfeed-hydra))

(use-package elfeed-org
  :ensure t
  :after (elfeed)
  :config
  (setq rmh-elfeed-org-files (list"~/.emacs.d/elfeed.org")))

(defhydra yk/hydra-elfeed ()
  ("q" (quit-window) "Quit")
  ("e" (elfeed-search-set-filter "@3-days-ago +emacs +unread") "Emacs")
  ("n" (elfeed-search-set-filter "@3-days-ago +news +unread") "News")
  ("t" (elfeed-search-set-filter "@3-days-ago +tech +unread") "Tech")
  ("r" (elfeed-search-set-filter "@3-days-ago +reddit +unread") "Reddit")
  ("f" (elfeed-search-fetch-visible) "Refresh"))

(defun run-elfeed-hydra ()
  (interactive)
  (yk/hydra-elfeed/body))

(defun run-elfeed ()
  "Runs all the necessary actions and refreshes elfeed"
  (interactive)
  (elfeed-org)
  (elfeed)
  (elfeed-update))

(use-package wttrin
  :ensure t
  :defer 5
  :config
  (setq wttrin-default-cities '("Varna,Bulgaria" "Vancouver,Canada" "Maple Ridge,Canada" "Burnaby,Canada"))
  (setq wttrin-default-accept-language '("Accept-Language" . "en"))
  )

;; Set normal state
(evil-set-initial-state 'help-mode 'normal)

;; Rebind q to quit
(evil-define-key 'normal help-mode-map (kbd "q") (lambda ()
                                                (interactive)
                                                   (quit-window t)))
;; Skip around buttons
(evil-define-key 'normal help-mode-map (kbd "TAB") (lambda ()
                                                     (interactive)
                                                     (forward-button 1 t t)))

;; For package manager
 (define-key package-menu-mode-map (kbd "j") 'next-line)
 (define-key package-menu-mode-map (kbd "k") 'previous-line)
 (define-key package-menu-mode-map (kbd "l") 'package-menu-describe-package)
 (define-key package-menu-mode-map "i" 'package-menu-mark-install)
 (define-key package-menu-mode-map "x" 'package-menu-execute)
 (define-key package-menu-mode-map "u" 'package-menu-mark-upgrades)
(define-key package-menu-mode-map (kbd "q") (lambda ()
                                         (kill-current-buffer)))
 (define-key package-menu-mode-map "/" 'evil-search-forward)
 (define-key package-menu-mode-map "?" 'evil-search-backward)
 (define-key package-menu-mode-map "n" 'evil-search-next)
 (define-key package-menu-mode-map "N" 'evil-search-previous)

;;Bindings for org mode. Only valid in org buffers
(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-log-done 'time)
  (setq org-deadline-warning-days 14)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-span (quote 7))
  (setq org-agenda-start-day "-1d")
  (setq org-agenda-remove-tags t)
  (setq org-tag-alist '(("@school" . ?s) ("@home" . ?h) ("@errand" . ?e) ("@goal" . ?g)))
  (setq
   org-startup-indented t

   ;;hide bold,italics...
   org-hide-emphasis-markers t

   ;; Hide leading stars. Looks better
   org-hide-leading-stars t

   ;; Open file in current buffer, not split
   org-link-frame-setup '((file . find-file))

   ;; Capture templates
   org-capture-templates
   '(("t" "Todo entry" entry (file+headline "~/Org/Agenda.org" "Today")
      "* TODO %?" :kill-buffer t)
     ("m" "Maybe entry" entry (file+headline "~/Org/Agenda.org" "Maybe Today")
      "* MAYBE %?" :kill-buffer t)
     ("s" "School Question" entry (file+headline "~/Org/School.org" "Questions")
      "* QUESTION %?" :kill-buffer t :prepend t)
     ("r" "Research/Read About" entry (file+headline "~/Wiki/ProjectIdeas/ToResearch.org" "To Find Out")
      "* RESEARCH %?" :kill-buffer t :prepend t)
     ("p" "Project Idea" entry (file+headline "~/Wiki/ProjectIdeas/ProjectIdeas.org" "Project Ideas")
      "* TODO %?" :kill-buffer t :prepend t)
     ("f" "Books" entry (file+headline "~/Org/Agenda.org" "Current Reading List")
      "** INSERT \n %(helm-org-wiki--get-org-link)")))



  ;; Do not split lines on a new todo
  (setq org-M-RET-may-split-line '((default . nil)))

  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.pdf\\'" . "zathura %s") 
          ("\\.epub\\'" . "zathura %s")
          ("\\.djvu\\'" . "zathura %s")))

  ;; Add syntax highlight to blocks
  (setq org-src-fontify-natively t)

  ;;Native tabs in src block
  (setq org-src-tab-acts-natively t)

  ;; Dont ask to run code, simply do it
  (setq org-confirm-babel-evaluate nil)

  ;; What languages to eval in source blocks
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (latex . t)
     (python . t)
     (C . t)
     (shell . t)
     (js . t)
     (haskell . t)
     (emacs-lisp . t)
     (scheme . t)
     (lisp . t)))


  ;;------Org Mode Bindings
  ;;Insert todo heading(inserts new line, inserts heading then enters insert mode)
  (evil-leader/set-key-for-mode 'org-mode "dd" 'org-todo)

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
                                                   (org-cycle)
                                                   (kill-buffer "*Calendar*")
                                                   (evil-append-line 1)))
  ;; Way too much to explain. Very important
  (evil-leader/set-key-for-mode 'org-mode "dr" 'org-ctrl-c-ctrl-c)

  ;; Insert a deadline for some item(usually todo's)
  (evil-leader/set-key-for-mode 'org-mode "di" #'(lambda ()
                                                   (interactive)
                                                   (org-deadline 1)
                                                   (org-cycle)
                                                   (kill-buffer "*Calendar*")
                                                   (evil-append-line 1)))
  ;;Compilation menu
  (evil-leader/set-key-for-mode 'org-mode "dc" 'org-export-dispatch)

  ;; Edit code blocks with syntax highlighting and so on
  (evil-leader/set-key-for-mode 'org-mode "de" 'org-edit-special)

  ;;Clock in
  (evil-leader/set-key-for-mode 'org-mode "oi" 'org-clock-in)
  ;; Clock out
  (evil-leader/set-key-for-mode 'org-mode "oo" 'org-clock-out)
  ;; Cancel
  (evil-leader/set-key-for-mode 'org-mode "os" 'org-clock-cancel)

  ;; Navigation
  (define-key org-mode-map (kbd "M-j") 'org-forward-heading-same-level)
  (define-key org-mode-map (kbd "M-k") 'org-backward-heading-same-level)
  (define-key org-mode-map (kbd "M-h") 'yav-go-up-org-heading)

  )

;;Open the agenda from anywhere
(evil-leader/set-key "oa" 'org-agenda-list)

;;Org capture
(evil-leader/set-key "oc" 'org-capture)

(setq org-todo-keywords'((sequence "TODO(t)" "NEXT(n)" "DOING(d)" "MAYBE(m)" "WAITING(w@)"
                                   "|" "DONE(D)" "CANCELED(C)")
                         (sequence "HABIT(h)" "DOING(d)" "|" "DONE(D)"
                                   "CANCELED(C)")
                         (sequence "ASSIGNMENT(a)" "DOING(d)" "|" "DONE(D)"
                                   "CANCELED(C)")
                         (sequence "TEST(t)" "|" "DONE(D)")
                         (sequence "HACK(H)" "|" "Done(D)")))

(setq org-agenda-files (list
                        "~/Org/Agenda.org"
                        "~/Org/Habits.org"
                        "~/Org/School.org"))

;;Org capture file
(setq org-default-notes-file "~/Org/OrgCaptures.org")

;; Open agenda in full window
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit t)

;;Make it so agenda opens horizontally
;; (setq split-height-threshold 80)
;; (setq split-width-threshold nil)
;; (setq org-agenda-window-frame-fractions '(0.7 . 0.8))

;; Skip done deadlines
(setq org-agenda-skip-deadline-if-done t)


;;Helps organize the agenda view
(use-package org-super-agenda
  :ensure t
  :after org
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "Daily" :todo "HABIT")
          (:name "Working On" :todo ("DOING" "WAITING"))
          (:name "Todo" :todo ("TODO" "NEXT"))
          (:name "School" :todo ("TEST" "ADMIN" "ASSIGNMENT" "SCHOOL"))
          (:name "Hack On" :todo "HACK")
          (:name "Maybe" :todo "MAYBE")))

  ;;Bindings for the agenda view itself(not valid in org mode!!!)
  (define-key org-agenda-keymap "q" 'org-agenda-exit)
  (define-key org-agenda-keymap "j" 'org-agenda-next-item)
  (define-key org-agenda-keymap "k" 'org-agenda-previous-item)
  (define-key org-agenda-keymap "d" 'org-agenda-todo)
  (define-key org-agenda-keymap "h" 'org-agenda-earlier)
  (define-key org-agenda-keymap "l" 'org-agenda-later)
  (define-key org-agenda-keymap (kbd "C-j") 'org-agenda-next-line)
  (define-key org-agenda-keymap (kbd "C-k") 'org-agenda-previous-line)
  (evil-leader/set-key-for-mode 'org-agenda-mode "di" 'org-agenda-clock-in)
  (evil-leader/set-key-for-mode 'org-agenda-mode "do" 'org-agenda-clock-out)
  (evil-leader/set-key-for-mode 'org-agenda-mode "dc" 'org-agenda-clock-cancel)
  (evil-leader/set-key-for-mode 'org-agenda-mode "df" 'org-agenda-filter-by-tag)
  )

;;Provides mathematical symbols in org mode
(use-package company-math
  :ensure t
  :defer t)

;; Journaling mode
(use-package org-journal
  :ensure t
  :config
  (setq org-journal-carryover-items nil)
  (setq org-journal-dir "~/Org/Others/Journal")
  (setq org-journal-find-file 'find-file)

  (evil-leader/set-key "]t" 'org-journal-new-entry)
  (add-hook 'org-journal-after-entry-create-hook 'org-journal-mode)
  (evil-leader/set-key-for-mode 'org-journal-mode "dj" 'org-journal-next-entry)
  (evil-leader/set-key-for-mode 'org-journal-mode "dk" 'org-journal-previous-entry)
  (evil-leader/set-key-for-mode 'org-journal-mode "ds" 'org-journal-search)
  ;; Override default behaviour. Was a pain in the ass to execute a buffer local hook.
  (evil-leader/set-key-for-mode 'org-journal-mode "wk" (lambda ()
                                                         (interactive)
                                                         (save-buffer)
                                                         (kill-buffer-and-window))))



;; Provides async execution of blocks
(use-package ob-async
  :ensure t
  :after (org))

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (org-bullets-mode 1))))

(use-package org-download
  :ensure t
  :after (org)
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))


(defun my-agenda-view ()
  "Open agenda and go to daily view without any interaction with the menus."
  (interactive)
  (org-agenda-list)
  (org-agenda-day-view)
  )

(use-package aggressive-indent
  :ensure t
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook 'aggressive-indent-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp)
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet t)
  (setq lsp-enable-xref t)
  (setq lsp-enable-folding t)
  (setq lsp-enable-indentation nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-file-watchers t)
  )

;;frontend for completions
(use-package company
  :ensure t
  :config
  (setq company-idle-delay .1)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-show-numbers t)

  ;;Keybindings for company selections
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle))

;;This company backend is used for language servers
(use-package company-lsp
    :commands company-lsp
    :ensure t
    :config
    (push 'company-lsp company-backends)
    (setq
     company-lsp-cache-candidates 'auto
     company-lsp-async t
     company-lsp-enable-snippet t
     company-lsp-enable-recompletion t))


;; Keeps a file containing the most used completions
(use-package company-statistics
  :ensure t
  :after company)


(use-package company-c-headers
  :ensure t
  :after company-lsp
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-enable nil
        lsp-ui-doc-mode t))

(use-package company-box
  :ensure t)
  ;; :hook (company-mode . company-box-mode))

(use-package origami
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'origami-mode))

;; Custom  function to add a project to lsp workspaces or ignore it
(defun add-to-lsp-workspace?()
  "If lsp cannot locate the folder for the project root, ask the
   user to either specify one or to not use lsp.  If it is found,
   ask if it should be used."
  (interactive)
  (if (not (lsp-workspace-root (buffer-file-name)))
      (if (y-or-n-p "No workspace found! Add to workspaces and run LSP or skip LSP entirely?")
          (progn
            (lsp-workspace-folders-add (read-string "Add the path:"))
            (lsp)
            )
        (message "LSP not setup!")
        )
    (progn 
      (message "Root found! Start LSP!."))
    )
  )

(use-package geiser
  :ensure t
  :after scheme-mode
  :hook
  (add-hook 'geiser-mode-hook 'rainbow-delimiters-mode))

;;Activate company mode in lisp mode
(use-package slime-company
:ensure t
:defer t)

;;Set up slime
(use-package slime
:ensure t
:mode ("\\.cl$" . lisp-mode) 
:config
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy slime-company)))

(use-package common-lisp-snippets
  :ensure t
  :after slime)

;;Elisp hook for auto complete
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (company-mode)
                                  (rainbow-delimiters-mode)))


;;Hook for common lisp. Starts up the REPL
(add-hook 'lisp-mode-hook #'(lambda ()
                              (company-mode)
                              (slime)
                              (require 'common-lisp-snippets)
                              (company-statistics-mode)
                              (yas-minor-mode)))

(use-package rustic
  :ensure t
  :config
  (setq rustic-rls-pkg nil)
  (setq rustic-lsp-server 'rust-analyzer)
  )

(use-package haskell-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.l?hs$" . haskell-mode)))

;;Default emacs python mode, set up a hook for it to enable elpy
(use-package python
  :ensure t
  :mode ("\\.py$" . python-mode))

(use-package lsp-python-ms
  :ensure t
  :after python)

 (use-package py-autopep8
  :ensure t
  :hook (python-mode . py-autopep8-enable-on-save))

 (add-hook 'python-mode-hook (lambda ()
                               ;;(add-to-lsp-workspace?)
                               (add-to-lsp-workspace?)
                               (origami-mode)
                               (yas-minor-mode)))

;; (use-package basic-c-compile
;;   :ensure t
;;   :defer t)

;; (use-package company-irony-c-headers
;;   :ensure t
;;   :after (company-irony)
;;   :config
;;   (add-to-list 'company-backends 'company-irony-c-headers))

;; (use-package company-irony
;;             :ensure t
;;             :config
;;             (require 'company)
;;             (setq company-irony-ignore-case 'smart)
;;             (add-to-list 'company-backends 'company-irony))

;; (use-package irony
;;             :ensure t
;;             :config
;;             (add-hook 'c-mode-hook 'irony-mode)
;;             (add-hook 'c++-mode-hook 'irony-mode)
;;             (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;             (evil-leader/set-key-for-mode 'c-mode "dp" 'irony-parse-buffer)
;;             (evil-leader/set-key-for-mode 'c++-mode "dp" 'irony-parse-buffer))

;; (add-hook 'c-mode-hook (lambda ()
;;                 (company-mode)
;;                 (yas-minor-mode)
;;                 (company-statistics-mode)
;;                 (flycheck-mode)))

;; (add-hook 'c++-mode-hook (lambda ()
;;                 (company-mode)
;;                 (yas-minor-mode)
;;                 (company-statistics-mode)
;;                 (flycheck-mode)))

;; (defun irony-parse-buffer ()
;;   "Parses the current buffer for irony mode to provide completions"
;;   (interactive)
;;   (irony--run-task-asynchronously (irony--parse-task)
;;                                   (lambda (result))))

(use-package ccls
  :ensure t
  :config (setq ccls-executable "/usr/bin/ccls"))

(add-hook 'c-mode-hook (lambda ()
                         (require 'ccls)
                         (add-to-lsp-workspace?)
                         (origami-mode)
                         (company-statistics-mode)))

(add-hook 'c++-mode-hook (lambda ()
                           (require 'ccls)
                           (add-to-lsp-workspace?)
                           (origami-mode)
                           (company-statistics-mode)))

(use-package js2-mode
  :ensure t
  :after js-mode
  :mode "\\.js$'")

(use-package tide
  :ensure t
  :after js2-mode)


(use-package js2-refactor
  :ensure t
  :after js2-mode)

(use-package skewer-mode
  :ensure t
  :after js2-mode)

  (add-hook 'js2-mode-hook (lambda ()
                              (tide-setup)
                              (tide-mode)
                              (eldoc-mode +1)
                              (flycheck-mode +1)
                              (origami-mode)
                              (tide-hl-identifier-mode +1)
                              (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
                              (company-mode +1)))

(use-package nasm-mode
:ensure t
:init
(add-hook 'asm-mode-hook 'nasm-mode))

(use-package company-web
:ensure t
:config
(require 'company-web-html))

(use-package web-mode
:ensure t
:config
(require 'company-web-html))
(add-hook 'css-mode-hook #'(lambda ()
			(company-mode)))

;; Set up latex
(use-package tex
  :mode "\\.tex$"
  :ensure auctex)

(use-package go-mode
  :ensure t
  :mode ("\\.go$" . go-mode))

;;Remove some of the default tool bars and scroll bars   
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Remove splash screen and startup message
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t)


;;Smooth scrolling
(setq scroll-conservatively 100)

;;Install theme packages
(use-package monokai-theme
  :ensure t
  :defer t)

(use-package spacemacs-theme
  :ensure t
  :defer t)

;; Current theme to use
(use-package doom-themes
  :ensure t
  :config
  (doom-themes-org-config)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-molokai-brighter-comments t
        doom-dracula-brighter-comments t
        doom-dracula-colorful-headers t))

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq
   doom-modeline-icon t
   doom-modeline-minor-modes nil
   doom-modeline-lsp t
   doom-modeline-buffer-modification-icon t
   doom-modeline-major-mode-icon t
   doom-modeline-buffer-file-name-style 'file-name))



(use-package kaolin-themes
  :ensure t
  :config
  (setq kaolin-bold t
        kaolin-hl-line-colored t
        kaolin-themes-comments-style 'normal)
  (load-theme 'kaolin-galaxy t)
  )


(set-face-attribute 'org-block-begin-line nil :foreground "#e6e6e8" :background "#2a2931")
(set-face-attribute 'org-block-end-line nil :foreground "#e6e6e8" :background "#2a2931")
