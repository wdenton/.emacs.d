;;;
;;; Sonic Pi (https://github.com/repl-electric/sonic-pi.el)
;;;

;; (require 'sonic-pi)

;; (add-hook 'sonic-pi-mode-hook
;; 	  (lambda ()
;; 	    (setq sonic-pi-path                   "/usr/lib/sonic-pi/")
;; 	    (setq sonic-pi-server-bin             "server/bin/sonic-pi-server.rb")
;; 	    ;; (setq sonic-pi-compile-extensions-bin "app/server/bin/compile-extensions.rb")
;; 	    (define-key ruby-mode-map "\C-c\C-c" 'sonic-pi-send-buffer)))

;; (setq false nil)

(setq sonic-pi-path "/usr/lib/sonic-pi/")

;; .spi seems to be the suffix for Sonic Pi scripts
(setq auto-mode-alist (cons '("\\.spi" . ruby-mode) auto-mode-alist))

(provide 'setup-sonic-pi)
