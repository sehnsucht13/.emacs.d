;;Kick up the garbage collection threshold. The let expressions will only bind it for the current scope and then everything will go back to normal
(let ((gc-cons-threshold most-positive-fixnum)
	  (file-name-handler-alist nil))

;;stops checking for file types when loading init
;; (let ((file-name-handler-alist nil))
 ;; Kick up garbage collection threshold
;; (setq gc-cons-threshold 100000000)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;Org file which contains all of the setup
(org-babel-load-file (expand-file-name "~/.emacs.d/newInit.org"))

;;-------------------------------------------------- Don't touch below --------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eclim-eclipse-dirs (quote ("/usr/lib/eclipse")))
 '(eclim-executable "/usr/lib/eclipse/eclim")
 '(org-agenda-files
   (quote
	("~/Org/Todo.org" "~/Org/Habits.org" "~/Org/School.org")))
 '(org-super-agenda-mode t)
 '(package-selected-packages
   (quote
	(helm-projectile projectile atom-one-dark-theme indium company-emacs-eclim eclim meghanada org-super-agenda slime-company yasnippet-snippets web-mode use-package treemacs-evil tide slime rjsx-mode racer pomodoro pdf-tools pandoc-mode nasm-mode intero helm evil-nerd-commenter evil-leader elpy doom-themes company-web company-statistics company-irony common-lisp-snippets cargo))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ))
