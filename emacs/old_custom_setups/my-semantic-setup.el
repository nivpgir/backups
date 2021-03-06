(provide 'my-semantic-setup)

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



