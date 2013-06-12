;; Emacs Speaks Statistics (for R)

; (autoload 'ess-site "ess-site" "R" t)
(require 'ess-site)
(setq ess-use-auto-complete t)
;; Make R go to the width of the Emacs frame
(add-hook 'ess-R-post-run-hook 'ess-execute-screen-options)

; (autoload 'ess-R-object-popup "ess-R-object-popup" "Get info for object at point, and display it in a popup" t)
; (require 'ess-R-object-popup)
(define-key ess-mode-map "\C-c\C-g" 'ess-R-object-popup)

; Make all help buffers go into one frame
(setq ess-help-own-frame 'nil)

(provide 'setup-ess)
