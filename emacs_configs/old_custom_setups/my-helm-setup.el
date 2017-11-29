(provide 'my-helm-setup)

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)


(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

;;fix golden ration with helm
(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

;;make "M-x" run "helm-M-x"
(global-set-key (kbd "M-x") 'helm-M-x)

;;set helm resize mode and adjust the min and max size of helm buffer
(helm-autoresize-mode t)
(setq helm-autoresize-max-height 35)
(setq helm-autoresize-min-height 0)

;;configure the shoe kill ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;;configure helm-mini
(global-set-key (kbd "C-x b") 'helm-mini)

;;use helm's find-file instead of normal one
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;;use C-c h o for helm occur:
(global-set-key (kbd "C-c h o") 'helm-occur)


;;enable fuzzy matching:
(setq helm-M-x-fuzzy-match t)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(provide 'my-helm-setup)
