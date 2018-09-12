;;; Configuration:

;; change highlight parantheses to highlight all contained expression
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; globally center on the cursor
;;(global-centered-cursor-mode +1)
;;(spacemacs/toggle-centered-point-globally-on)

;;enable company-flx for fuzzy completion in complay-idle-completion
;;(with-eval-after-load 'company
;;  (company-flx-mode +1))

;;company fuzzy search
(setq company-search-regexp-function (lambda (search-term)
                                       (mapconcat 'identity (seq-map 'string search-term) ".*")
                                       )
      )
;; to remove currnetly selected region on type without having to manually delete
(delete-selection-mode)

;; set chrome as the default browser
(setq browse-url-browser-function 'browse-url-chrome)

;; make backups in a specified backups directory instead of the same directory of the file
(setq make-backup-files t)
(setq backup-directory-alist '((".*" . "~/.emacs-backup")))


;; the background of the expression highlighting with show paren mode
;; is really ugly with the idea-darkula mode so we change it here
(custom-theme-set-faces
 'idea-darkula
 '(show-paren-match ((t (:background "dark slate gray")))))

