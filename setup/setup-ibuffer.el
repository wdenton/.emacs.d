;; ibuffer is a nicer way of showing the buffer list (C-x C-b)

;; ibuffer-vc extends it to automatically group buffers by the parent
;; version control directory
;; See https://github.com/purcell/ibuffer-vc

;; Alias the usual buffer list command to ibuffer
(defalias 'list-buffers 'ibuffer)

(add-hook 'ibuffer-hook
	  (lambda ()
	    (ibuffer-vc-set-filter-groups-by-vc-root)
	    (unless (eq ibuffer-sorting-mode 'alphabetic)
	      (ibuffer-do-sort-by-alphabetic))))

(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
	      (name 18 18 :left :elide)
	      " "
	      (size 9 -1 :right)
	      " "
	      (mode 16 16 :left :elide)
	      " "
	      (vc-status 16 16 :left)
	      " "
	      filename-and-process)))

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
