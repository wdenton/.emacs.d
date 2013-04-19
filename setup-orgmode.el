; Nice bullets!
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

; Hide the /italics/ and *bold* markers
(setq org-hide-emphasis-markers t)

; Hit return on a link to open it in a browser
(setq org-return-follows-link t)

(setq org-display-inline-images t)

; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (latex . t)
   (R . t)
   (ruby . t)
   (sh . t)
   (sql . t)
   )
 )

(provide 'setup-orgmode)
