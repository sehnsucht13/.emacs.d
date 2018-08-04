;;Use-package bootstrap for new install
(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))


;; Always ensure all packages
(setq use-package-always-ensure t)

;; Logging
;; (setq use-package-verbose t)

;; more logging
;; (setq use-package-compute-statistics t)

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

  ;;Install theme package
  (use-package monokai-theme
    :ensure t)

  ;;Theme settings
  (load-theme 'wombat t)

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
(setf initial-buffer-choice #'(lambda () (find-file "~/Org/Agenda.org")))

(setq fill-column 80)

;; Visual indicators for wrap lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;;Wrap lines so they do not go past screen edge
(global-visual-line-mode 1)

;;Stop keys being echoed in minibuffer. Messes up which-key
(setq echo-keystrokes 0)

;; Clear image cache sooner to save memory on larger pdfs. Measured in seconds.
(setq image-cache-eviction-delay 15)

;;Always focus help buffers
(setq help-window-select t)

;;Set initial buffer mode to text mode.
(setq initial-major-mode 'fundamental-mode)

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
(evil-leader/set-key "ws" 'helm-mini)

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
  :after (evil-leader)
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

;;Resizing of windows. (C is the control key)
(define-key evil-normal-state-map (kbd "<C-left>") 'evil-window-decrease-width)
(define-key evil-normal-state-map (kbd "<C-right>") 'evil-window-increase-width)
(define-key evil-normal-state-map (kbd "<C-up>") 'evil-window-increase-height)
(define-key evil-normal-state-map (kbd "<C-down>") 'evil-window-decrease-height)


;;Enter console in Insert state
(evil-set-initial-state 'ansi-term 'insert)

;;Visual lines
(define-key evil-normal-state-map (kbd "C-j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "C-k") 'evil-previous-visual-line)

;;Exit out of brackets while in insert mode
(define-key evil-insert-state-map (kbd "C-a") 'exit-bracket)

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

(defun minibuffer-increase-threshold ()
  "Small function I stole from somebodys init. Used for entering the minibuffers for autocomplete/fuzzy searching and simply increases the threshold"
  (setq gc-cons-threshold most-positive-fixnum))

(defun minibuffer-normal-threshold ()
  "Another small function i stole. Instead of increasing the gc threshold, it brings it to normal(that is 800 KB)"
  (setq gc-cons-threshold 1000000))

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
"Exit out of the brackets and goes to the end of the line"
(interactive)
(evil-normal-state 1)
(evil-append-line 1))

(defun start-external-shell ()
"Start an external shell, whatever the default system shell is"
(interactive)
(start-process "shell-process" nil "xfce4-terminal"))

(defun org-create-heading (headingSize)
  (interactive "p")
  (evil-normal-state 1)
  (evil-open-below 1)
  (insert (make-string headingSize ?*)))

(defun start-which-key-for-mode ()
  "Start which key and display the bindings for the current mode"
  (interactive)
  (which-key-mode)
  (setq which-key-idle-delay 0)
  (which-key-show-major-mode))


(defun start-which-key-for-full-keymap ()
  "Start which key and display the bindings for the full keymap available"
  (interactive)
  (which-key-mode)
  (setq which-key-idle-delay 0)
  (which-key-show-full-keymap))

(defun stop-which-key-for-all ()
  "Disable which key for the current buffer. Turns off the minor mode and resets the idle delay"
  (interactive)
  (which-key-mode nil)
  (setq which-key-idle-delay 3.0))

(defun make-my-bookmark ()
  "Automatically creates a bookmark with the name Current + filename"
  (interactive)
  (bookmark-set (buffer-name)))

(load-file "~/pprojects/helm-org-wiki/helm-org-wiki.el")

(evil-leader/set-key  "ti" 'helm-org-wiki-open-index)
(evil-leader/set-key "tw" 'helm-org-wiki-walk-wiki)
(evil-leader/set-key "tn" 'helm-org-wiki-create-new-article)

(evil-leader/set-key-for-mode 'org-mode "ih" 'wiki-haskell-block)
(evil-leader/set-key-for-mode 'org-mode "ija" 'wiki-java-block)
(evil-leader/set-key-for-mode 'org-mode "ijs" 'wiki-javascript-block)
(evil-leader/set-key-for-mode 'org-mode "ip" 'wiki-python-block)
(evil-leader/set-key-for-mode 'org-mode "ic" 'wiki-C-block)
(evil-leader/set-key-for-mode 'org-mode "iv" 'wiki-C++-block)
(evil-leader/set-key-for-mode 'org-mode "ir" 'wiki-rust-block)
(evil-leader/set-key-for-mode 'org-mode "ie" 'wiki-emacs-lisp-block)
(evil-leader/set-key-for-mode 'org-mode "ila" 'wiki-latex-block)
(evil-leader/set-key-for-mode 'org-mode "ilp" 'wiki-lisp-block)
(evil-leader/set-key-for-mode 'org-mode "is" 'wiki-sh-block)

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
                                         (quit-window t)))
 (define-key package-menu-mode-map "/" 'evil-search-forward)
 (define-key package-menu-mode-map "?" 'evil-search-backward)
 (define-key package-menu-mode-map "n" 'evil-search-next)
 (define-key package-menu-mode-map "N" 'evil-search-previous)

(use-package helm
      :ensure t
      :init
      ;; Enable helm mode
      (helm-mode 1)
      :config
      (setq helm-mode-fuzzy-match t)
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

;;Snippets manager
(use-package yasnippet
  :ensure t
  :defer 3
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
  (evil-leader/set-key "pc" 'projectile-commander)
  (evil-leader/set-key "pk" 'projectile-kill-buffers)
  (projectile-mode 1)
  (setq projectile-enable-caching t))

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
;; Finds a file within project
(evil-leader/set-key "pf" 'helm-projectile-find-file)
;; Finds a directory and opens it within project
(evil-leader/set-key "pd" 'helm-projectile-find-dir)
;; Switches to a project buffer
(evil-leader/set-key "pb" 'helm-projectile-switch-to-buffer))

(defun yav-go-up-org-heading ()
"Go up to the parent heading and automatically close it"
(interactive)
  (outline-up-heading 1))

;;Open the agenda from anywhere
(evil-leader/set-key "oa" 'org-agenda)

;;Org capture
(evil-leader/set-key "oc" 'org-capture)

;;Org mode todo states
(setq org-todo-keywords '((sequence "TODO(t)" "MAYBE(m)" "WAITING(w)" "CURRENT(f)" "NEXT(n)"  "|" "DONE(d)" "CANCELLED(c)")))

;;Org capture file
(setq org-default-notes-file "~/Org/OrgCaptures.org")

;;Make it so agenda opens horizontally
(setq split-height-threshold 40)
(setq split-width-threshold nil)
(setq org-agenda-window-setup 'reorganize-frame)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-window-frame-fractions '(0.7 . 0.8))
(setq org-agenda-skip-deadline-if-done t)

;;Bindings for org mode. Only valid in org buffers
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-log-done 'time)
  (setq org-deadline-warning-days 18)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-span (quote 7))
  (setq org-agenda-start-day "-1d")
  (setq org-agenda-remove-tags t)
  (setq org-tag-alist '(("@school" . ?s) ("@home" . ?h) ("@errand" . ?e) ("@goal" . ?g)))
  ;; start indented
  (setq org-startup-indented t)
  ;;hide bold,italics...
  (setq org-hide-emphasis-markers t)
  ;; Hide leading stars. Looks better
  (setq org-hide-leading-stars t)
  ;; Open file in current buffer, not split
  (setq org-link-frame-setup '((file . find-file)))
  :config
  ;; Capture templates
  (setq org-capture-templates
        '(("t" "Todo entry" entry (file+headline "~/Org/Agenda.org" "Today")
           "* TODO %?" :kill-buffer t)
          ("m" "Maybe entry" entry (file+headline "~/Org/Agenda.org" "Maybe Today")
           "* MAYBE %?" :kill-buffer t)
          ("s" "School question" entry (file+headline "~/Org/School.org" "Questions")
           "* QUESTION %?" :kill-buffer t :prepend t)
          ("r" "Research/Read About" entry (file+headline "~/Wiki/ProjectIdeas/ToResearch.org" "To Find Out")
           "* RESEARCH %?" :kill-buffer t :prepend t)
          ("p" "Project Idea" entry (file+headline "~/Wiki/ProjectIdeas/ProjectIdeas.org" "Project Ideas")
                                                   "* TODO %?" :kill-buffer t :prepend t)))

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

;;Helps organize the agenda view
  (use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
          '((:name "Today" :todo "TODO")
            (:name "School" :todo ("TEST" "ADMIN" "ASSIGNMENT"))
            (:name "Maybe" :todo "MAYBE"))))

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
  (evil-leader/set-key-for-mode 'org-journal-mode "dj" 'org-journal-next-entry)
  (evil-leader/set-key-for-mode 'org-journal-mode "dk" 'org-journal-previous-entry)
  (evil-leader/set-key-for-mode 'org-journal-mode "ds" 'org-journal-search)
  ;; Override default behaviour. Was a pain in the ass to execute a buffer local hook.
  (evil-leader/set-key-for-mode 'org-journal-mode "wk" (lambda ()
                                                         (interactive)
                                                         (save-buffer)
                                                         (kill-buffer-and-window))))


   ;;Bindings for the agenda view itself(not valid in org mode!!!)
(define-key org-agenda-mode-map "q" 'org-agenda-exit)
(define-key org-agenda-mode-map "j" 'org-agenda-next-item)
(define-key org-agenda-mode-map "k" 'org-agenda-previous-item)
(define-key org-agenda-mode-map "d" 'org-agenda-todo)
(define-key org-agenda-mode-map "h" 'org-agenda-earlier)
(define-key org-agenda-mode-map "l" 'org-agenda-later)
(define-key org-agenda-mode-map (kbd "C-j") 'org-agenda-next-line)
(define-key org-agenda-mode-map (kbd "C-k") 'org-agenda-previous-line)
(evil-leader/set-key-for-mode 'org-agenda-mode "di" 'org-agenda-clock-in)
(evil-leader/set-key-for-mode 'org-agenda-mode "do" 'org-agenda-clock-out)
(evil-leader/set-key-for-mode 'org-agenda-mode "dc" 'org-agenda-clock-cancel)
(evil-leader/set-key-for-mode 'org-agenda-mode "df" 'org-agenda-filter-by-tag)

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

(use-package org-noter
  :ensure t
  :defer t)

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

(use-package pdf-tools
  :ensure t
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :config
    (pdf-tools-install)
    (setq pdf-view-continuous t)
    (setq pdf-view-display-size 'fit-width)
    (evil-set-initial-state 'pdf-view-mode 'normal)
    (evil-define-key 'normal pdf-view-mode-map (kbd "j") 'evil-collection-pdf-view-next-line-or-next-page
      (kbd "k") 'evil-collection-pdf-view-previous-line-or-previous-page
      (kbd "J") 'pdf-view-next-page
      (kbd "K") 'pdf-view-previous-page
      (kbd "i") 'pdf-outline
      (kbd "q") 'bury-buffer
      (kbd "Q") 'kill-current-buffer
      (kbd "gg") 'pdf-view-first-page
      (kbd "G") 'evil-collection-pdf-view-goto-page))


(setq doc-view-continuous t)
(evil-set-initial-state 'doc-view-mode 'normal)
      (evil-define-key 'normal doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page
        (kbd "k") 'doc-view-previous-line-or-previous-page
        (kbd "J") 'doc-view-next-page
        (kbd "K") 'doc-view-previous-page
        (kbd "q") 'bury-buffer
        (kbd "Q") 'kill-current-buffer
        (kbd "gg") 'doc-view-first-page
        (kbd "G") 'doc-view-last-page)

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
  :after (flycheck))

(use-package magit
  :ensure t
  :defer t
  :init
  (evil-leader/set-key "ms" 'magit-status)
  (evil-leader/set-key "mp" 'magit-push)
  (evil-leader/set-key "mc" 'magit-commit)
  (evil-leader/set-key "md" 'magit-pull))

(use-package magithub
  :ensure t
  :after (magit))

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
  "filter . take 1"
  ("e" (elfeed-search-set-filter "@1-week-ago +emacs +unread") "Emacs")
  ("n" (elfeed-search-set-filter "@3-days-ago +news +unread") "News")
  ("t" (elfeed-search-set-filter "@1-week-ago +tech +unread") "Tech")
  ("f" (elfeed-search-fetch) "Refresh"))

(defun run-elfeed-hydra ()
  (interactive)
  (yk/hydra-elfeed/body))

(defun run-elfeed ()
  "Runs all the necessary actions and refreshes elfeed"
  (interactive)
  (elfeed-org)
  (elfeed)
  (elfeed-update))

;;Display tooltips for functions. Only activated in emacs lisp mode
(use-package company-quickhelp
  :ensure t
  :defer t)

;; ;;frontend for completions
(use-package company
            :ensure t
            :config
            (setq company-idle-delay 0)
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


;;Keeps a file containing the most used completions
(use-package company-statistics
:ensure t
:after (company))

(use-package lsp-mode
  :ensure t
  :init
  (add-hook 'c-mode-hook 'lsp-mode))

;;This company backend is used for language servers
(use-package company-lsp
  :ensure t
  :after (lsp-mode)
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-cache-candidates 'auto)
  (setq company-lsp-async t)
  (setq company-lsp-enable-recompletion t))

(use-package lsp-ui
  :ensure t
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode))

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
(add-hook 'lisp-mode-hook #'(lambda ()
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

;; Due to issues with installing ghc-mod on manjaro(and lack of support for new compiler), this will replace it.
(use-package haskell-snippets
  :ensure t
  :after (intero))

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
  :after (company-irony)
  :config
  (add-to-list 'company-backends 'company-irony-c-headers))

(use-package company-irony
            :ensure t
            :config
            (require 'company)
            (setq company-irony-ignore-case 'smart)
            (add-to-list 'company-backends 'company-irony))

(use-package irony
            :ensure t
            :config
            (add-hook 'c-mode-hook 'irony-mode)
            (add-hook 'c++-mode-hook 'irony-mode)
            (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
            (evil-leader/set-key-for-mode 'c-mode "dp" 'irony-parse-buffer)
            (evil-leader/set-key-for-mode 'c++-mode "dp" 'irony-parse-buffer))

(add-hook 'c-mode-hook (lambda ()
                (company-mode)
                (yas-minor-mode)
                (company-statistics-mode)
                (flycheck-mode)))

(add-hook 'c++-mode-hook (lambda ()
                (company-mode)
                (yas-minor-mode)
                (company-statistics-mode)
                (flycheck-mode)))

(defun irony-parse-buffer ()
  "Parses the current buffer for irony mode to provide completions"
  (interactive)
  (irony--run-task-asynchronously (irony--parse-task)
                                  (lambda (result))))

;; (defun enable-my-cquery ()
;;   (condition-case nil
;;       (lsp-cquery-enable)
;;     (user-error nil)))

;; (use-package cquery
;;   :ensure t
;;   :commands lsp-cquery-enable
;;   :init (add-hook 'c-mode-common-hook #'enable-my-cquery)
;;   :config
;;   (setq cquery-executable "/usr/bin/cquery")
;; (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack" :completion (:detailedLabel t))))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package tide
  :ensure t
  :after (js2-mode))


(use-package js2-refactor
  :ensure t
  :after (js2-mode))

(use-package skewer-mode
  :ensure t
  :after (js2-mode))

  (add-hook 'js2-mode-hook (lambda ()
                              (tide-setup)
                              (tide-mode)
                              (eldoc-mode +1)
                              (flycheck-mode +1)
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
  :defer t
  :ensure auctex)
