;;Kick up the garbage collection threshold. The let expressions will only bind it for the current scope and then everything will go back to normal
(let ((gc-cons-threshold most-positive-fixnum)
	  (file-name-handler-alist nil))

  ;; Repos to pull packages from
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (package-initialize)


;;Org file which contains all of the setup
(org-babel-load-file (expand-file-name "~/.emacs.d/newInit.org")))

;;-------------------------------------------------- Don't touch below --------------------------------------------------------
;; DO NOT wrap this in the parentheses for the garbage collection threshold. Any time a new package is installed, it will not be recognized and emacs will create another identical one.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(eclim-eclipse-dirs (quote ("/usr/lib/eclipse")))
 '(eclim-executable "/usr/lib/eclipse/eclim")
 '(org-agenda-files (quote ("~/Org/Agenda.org" "~/Org/School.org")))
 '(org-super-agenda-mode t)
 '(package-selected-packages
   (quote
	(org-board emacs-cquery flycheck-irony irony-eldoc elfeed elfeed-org org-bullets org-download neotree ob-go ob-typescript helm-org-rifle helm-dictionary anki-editor solarized-theme zenburn-theme ob-async esup org-noter tide nov auctex company-auctex org-plus-contrib ob-rust origami dired-filter dired-ranger org-journal cquery tomatinho helm-spotify-plus monokai-theme company-lsp git-gutter-fringe+ lsp-intellij lsp-ui magit helm-projectile projectile indium company-emacs-eclim meghanada org-super-agenda slime-company web-mode use-package treemacs-evil rjsx-mode racer pdf-tools pandoc-mode nasm-mode intero helm evil-nerd-commenter evil-leader elpy doom-themes company-web company-statistics company-irony common-lisp-snippets cargo)))
 '(pdf-cache-image-limit 15)
 '(pdf-cache-prefetch-delay 0.7)
 '(pdf-occur-global-minor-mode t)
 '(pdf-view-display-size (quote fit-width))
 '(pdf-view-use-imagemagick t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
