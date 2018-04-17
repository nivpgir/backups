(provide 'my-flycheck-setup)

;;flyscheck setup:
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-tip)
(flycheck-tip-use-timer 'verbose)


(provide 'my-flycheck-setup)
;;; my-flycheck-setup.el ends here(provide 'my-flycheck-setup)
;;; my-flycheck-setup.el ends here
