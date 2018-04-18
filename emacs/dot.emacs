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
   (quote (helm flycheck flycheck-tip nyan-mode golden-ratio-scroll-screen golden-ratio projectile projectile-codesearch projectile-speedbar company sr-speedbar stickyfunc-enhance ggtags geiser undo-tree volatile-highlights yasnippet))))
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
(require 'my-settings)
(require 'my-helm-setup)
