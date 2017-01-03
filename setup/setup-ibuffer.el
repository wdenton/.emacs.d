;; ibuffer is a nicer way of showing the buffer list (C-x C-b)

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

;; For ibuffer-projectile
;; https://github.com/purcell/ibuffer-projectile
(add-hook 'ibuffer-hook
	  (lambda ()
	    (ibuffer-projectile-set-filter-groups)
	    (unless (eq ibuffer-sorting-mode 'alphabetic)
	      (ibuffer-do-sort-by-alphabetic))))

(provide 'setup-ibuffer)
