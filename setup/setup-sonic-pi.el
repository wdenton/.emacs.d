;;;
;;; Sonic Pi (https://github.com/repl-electric/sonic-pi.el)
;;;

(require 'sonic-pi)

(add-hook 'sonic-pi-mode-hook
	  (lambda ()
	    (setq sonic-pi-path "/usr/local/src/sonic-pi/")
 	    ;; (setq sonic-pi-path "/usr/local/src/sonic-pi-2.6.0/")
	    (define-key ruby-mode-map "\C-c\C-c" 'sonic-pi-send-buffer)))

;; .spi seems to be the suffix for Sonic Pi scripts
(setq auto-mode-alist (cons '("\\.spi" . ruby-mode) auto-mode-alist))

(provide 'setup-sonic-pi)
