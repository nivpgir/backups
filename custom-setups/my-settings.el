(provide 'my-settings)

;;unbinding C-m from RET
(define-key input-decode-map [?\C-m] [C-m])
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
;;settings for gdb:
;;; use gdb-many-windows by default
(setq gdb-many-windows t)
;;; Non-nil means display source file containing the main routine at startup
(setq gdb-show-main t)


;;navigate using i,k,j,l keys as <up> <down> <left> <right> accordingly
(global-set-key (kbd "H-j") 'left-char)
(global-set-key (kbd "H-l") 'right-char)
(global-set-key (kbd "C-H-j") 'left-word)
(global-set-key (kbd "C-H-l") 'right-word)
(global-set-key (kbd "H-i") 'previous-line)
(global-set-key (kbd "H-k") 'next-line)



(provide 'my-settings)
;;; my-settings.el ends here
