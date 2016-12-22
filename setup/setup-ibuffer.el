;; ibuffer is a nicer way of showing the buffer list (C-x C-b)

;; ibuffer-vc extends it to automatically group buffers by the parent
;; version control directory
;; See https://github.com/purcell/ibuffer-vc

;; Alias the usual buffer list command to ibuffer
(defalias 'list-buffers 'ibuffer)

;; Set up some default groups so that files are grouped by type (or
;; location)
(setq ibuffer-saved-filter-groups
      (quote (("default"
 	       ("dired" (mode . dired-mode))
 	       ("emacs" (or
 			 (name . "^\\*scratch\\*$")
 			 (name . "^\\*Messages\\*$")))
	       ))))

;; (add-hook 'ibuffer-mode-hook
;; 	  (lambda ()
;; 	    (ibuffer-switch-to-saved-filter-groups "default")))

(provide 'setup-ibuffer)
