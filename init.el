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
	("53d2ec6a3c25d35869abcacfe5c061b280ca71cb473639ea569feca978a7d8a2" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "ae0193882f194278ed9613b2127ef8d913762a1ff6b5887248035bf584adc9aa" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "59e82a683db7129c0142b4b5a35dbbeaf8e01a4b81588f8c163bd255b76f4d21" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(eclim-eclipse-dirs (quote ("/usr/lib/eclipse")))
 '(eclim-executable "/usr/lib/eclipse/eclim")
 '(evil-search-module (quote evil-search))
 '(lsp-prefer-flymake nil)
 '(org-agenda-files (quote ("~/Org/Agenda.org")))
 '(org-super-agenda-mode t)
 '(package-selected-packages
   (quote
	(doom-modeline helm-rg srcery-theme geiser cyberpunk-2019-theme pipenv lsp-python-ms rainbow-delimiters ms-python org-journal spacemacs-themes cyberpunk-theme spacemacs-theme annotate evil-easymotion evil-lispy evil-magit evil-paredit flycheck-golangci-lint flycheck-haskell flycheck-rust go-gopath go-guru go-rename godoctor minibuffer-cua minibuffer-line company-c-headers lsp-haskell lsp-rust magit-todos go-mode go-snippets lsp ccls company-shell company-box lsp-python yasnippet org dashboard org-board emacs-cquery flycheck-irony irony-eldoc elfeed elfeed-org org-bullets org-download neotree ob-go ob-typescript helm-org-rifle helm-dictionary anki-editor solarized-theme ob-async esup org-noter tide nov auctex company-auctex ob-rust origami dired-filter dired-ranger cquery tomatinho helm-spotify-plus monokai-theme company-lsp git-gutter-fringe+ lsp-intellij lsp-ui magit helm-projectile indium company-emacs-eclim meghanada org-super-agenda slime-company web-mode use-package treemacs-evil rjsx-mode racer pdf-tools pandoc-mode nasm-mode intero helm evil-nerd-commenter evil-leader elpy doom-themes company-web company-statistics company-irony common-lisp-snippets cargo)))
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
