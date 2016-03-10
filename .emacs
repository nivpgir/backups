(add-to-list 'load-path "~/.emacs.d/custom-setups")

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
;;add melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote (flycheck flycheck-tip nyan-mode golden-ratio-scroll-screen golden-ratio projectile projectile-codesearch projectile-speedbar company sr-speedbar stickyfunc-enhance ggtags geiser undo-tree volatile-highlights yasnippet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;use my predefined setups in 'custom setups':
(require 'my-c-c++-setup)
(require 'my-semantic-setup)
(require 'my-projectile-setup)
(require 'my-golden-ratio-setup)
(require 'my-flycheck-setup)
(require 'my-custom-funcs)

;;use ibuffer instead of list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;set theme zenburn
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)
;;enable line numbers always
(global-linum-mode t)
;;yasnippet
(require 'yasnippet)
(yas-global-mode 1)
;;company
(add-hook 'after-init-hook 'global-company-mode)
;;setup the nyan-mode
(case window-system ((x w32) (nyan-mode)))
;;activate volatile highlights
(volatile-highlights-mode t)


;;setup semantic C/C++ completions
(require 'cc-mode)
(require 'semantic)
(require 'stickyfunc-enhance)
(global-semanticdb-minor-mode 1)
;;;parse even if not explicilty told to
(global-semantic-idle-scheduler-mode 1)
;;;show function in minibuffer
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)


(semantic-mode 1)

(semantic-add-system-include "/usr/include/boost")
(semantic-add-system-include "~/linux/kernel")
(semantic-add-system-include "~/linux/include")


;;enable projectile
(projectile-global-mode)

;;set style to "linux"
(setq c-default-style "linux")
;;set the offset to 4
(setq c-basic-offset 4)
;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)
;;use space to indent by default
(setq-default indent-tabs-mode t)
;;automatically put closing brackets
(electric-pair-mode 1)


;;folding in c/c++-mode
(add-hook 'c-mode-common-hook   'hs-minor-mode)

;;make thefolding shorcuts work
(add-hook 'prog-mode-hook 'hs-minor-mode)
