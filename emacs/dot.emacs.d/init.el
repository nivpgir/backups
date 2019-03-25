(defvar bootstrap-version)
(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/master/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))



;; My Functions and configs
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)
;;unbinding C-m from RET
(define-key input-decode-map [?\C-m] [C-m])
;;use ibuffer instead of list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;enable line numbers always
(global-linum-mode t)
;;setup the nyan-mode
;; (case window-system ((x w32) (nyan-mode)))
;;activate volatile highlights
;; (volatile-highlights-mode t)
;;make backup files only in "~/.saves":
(setq backup-directory-alist `(("." . "~/.saves")))
;;settings for gdb:
;;; use gdb-many-windows by default
(setq gdb-many-windows t)
;;; Non-nil means display source file containing the main routine at startup
(setq gdb-show-main t)

;; first things first...
;; delete selection mode
(delete-selection-mode t)


;; darcula theme
(straight-use-package 'idea-darkula-theme)
(load-theme 'idea-darkula)

;; ;; Company
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Helm
(straight-use-package 'helm)
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))
;;make "M-x" run "helm-M-x"
(global-set-key (kbd "M-x") 'helm-M-x)
;;set helm resize mode and adjust the min and max size of helm buffer
(helm-autoresize-mode t)
(setq helm-autoresize-max-height 35)
(setq helm-autoresize-min-height 35)
;;configure the show kill ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;;configure helm-mini
(global-set-key (kbd "C-x b") 'helm-mini)
;;use helm's find-file instead of normal one
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;;use C-c h o for helm occur:
(global-set-key (kbd "C-c h o") 'helm-occur)
(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("02591317120fb1d02f8eb4ad48831823a7926113fa9ecfb5a59742420de206e0" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; magit
(straight-use-package 'magit)

