(provide 'my-settings)

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
;;make backup files only in "~/.saves":
(setq backup-directory-alist `(("." . "~/.saves")))

(provide 'my-settings)
;;; my-settings.el ends here
