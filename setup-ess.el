;; Emacs Speaks Statistics (for R)
(require 'ess-site)
(setq ess-use-auto-complete t)
;; Make R go to the width of the Emacs frame
(add-hook 'ess-R-post-run-hook 'ess-execute-screen-options)

(require 'ess-R-object-popup)
(define-key ess-mode-map "\C-c\C-g" 'ess-R-object-popup)

(provide 'setup-ess)
