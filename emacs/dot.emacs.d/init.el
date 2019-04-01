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
(define-prefix-command 'my-keymap)
(global-set-key (kbd "M-m") 'my-keymap)
(defun compose (f g)
  `(lambda (x) (,f (,g x))))

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
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-set-key (kbd "C-c d") 'duplicate-line)
(defun alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))
(global-set-key (kbd "M-m <tab>") 'alternate-buffer)

;;unbinding C-m from RET
(define-key input-decode-map [?\C-m] [C-m])
;;use ibuffer instead of list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;enable line numbers always
(global-linum-mode t)
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
;; match parens ;; TODO: missing show full expression and change highlight color
(show-paren-mode t)
(setq show-paren-style 'expression)
(straight-use-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode)
;; centered point ;; if this will be problematic then consider using: https://www.emacswiki.org/emacs/centered-cursor-mode.el
;;(straight-use-package
;; '(centered-point-mode :type git :host github :repo "jmercouris/emacs-centered-point"))
;;(require 'centered-point-mode-autoloads)
;;(require 'centered-point-mode)
(defun line-change ()
  (recenter)
  )
(define-minor-mode centered-point-mode
  "Alaways center the cursor in the middle of the screen."
  :lighter "..."
  (cond (centered-point-mode (add-hook 'post-command-hook 'line-change))
	(t (remove-hook 'post-command-hook 'line-change)))
  )
(centered-point-mode t)
;; expand region with C-:
(straight-use-package 'expand-region)
(global-set-key (kbd "C-;") 'er/expand-region)
;; undo-tree with diff on visualizing
(straight-use-package 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-visualizer-diff t)
;; comment with C-/
(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "C-x C-;"))
(global-set-key (kbd "C-/") 'comment-line)
(define-key undo-tree-map (kbd "C-/") nil)
(setq winum-keymap
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-`") 'winum-select-window-by-number)
	(define-key map (kbd "M-0") 'winum-select-window-0-or-10)
	(define-key map (kbd "M-1") 'winum-select-window-1)
	(define-key map (kbd "M-2") 'winum-select-window-2)
	(define-key map (kbd "M-3") 'winum-select-window-3)
	(define-key map (kbd "M-4") 'winum-select-window-4)
	(define-key map (kbd "M-5") 'winum-select-window-5)
	(define-key map (kbd "M-6") 'winum-select-window-6)
	(define-key map (kbd "M-7") 'winum-select-window-7)
	(define-key map (kbd "M-8") 'winum-select-window-8)
	(define-key map (kbd "M-9") 'winum-select-window-9)
	map))
(straight-use-package 'winum)
(winum-mode)
;; darcula theme
(straight-use-package 'idea-darkula-theme)
(load-theme 'idea-darkula)
(custom-theme-set-faces
 'idea-darkula
 '(show-paren-match ((t (:background "dark slate gray")))))


;; Company
;; remember that navigating in the popup is done with M-n and M-p
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
    ("02591317120fb1d02f8eb4ad48831823a7926113fa9ecfb5a59742420de206e0" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; magit
(straight-use-package 'magit)
;;(straight-use-package 'magithub)



;;; Languages:
;; haskell
(straight-use-package 'haskell-mode)
(straight-use-package 'company-ghc)
(if (bound-and-true-p company-candidates)
    (add-to-list 'company-backends 'company-ghc))

;; ruby
;; c-c++
;; python
;; scala
;; also maybe:
;; elixir
;; nim
;; java

;; get something as emacs help (helpful or something)
