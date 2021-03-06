(provide 'my-c-c++-setup)

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


(provide 'my-c-c++-setup)
;;; my-c-c++-setup.el ends here
