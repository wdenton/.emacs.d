; Nice bullets!
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

; Hide the /italics/ and *bold* markers
(setq org-hide-emphasis-markers t)

; Hit return on a link to open it in a browser
(setq org-return-follows-link t)

; Shift and arrow keys to select text works a bit differenty in org-mode
(setq org-support-shift-select t)

; Embed an image with [[file:foo.png]] and then C-c C-x C-v to view
(setq org-display-inline-images t)
; Display images when a file is loaded (I can always toggle them off if I don't want them)
(add-hook 'org-mode-hook (lambda () (org-toggle-inline-images)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

; Capturing
(setq org-default-notes-file "~/notes.org") ; Change this when I use it for real
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("w" "Work todo" entry (file+headline "~/york/gtd/next-actions.org" "Tasks") "* TODO %?\n %u\n %a"))
      )

; Active Babel languages
; See http://orgmode.org/org.html#Languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (ditaa . t)
   (dot . t)
   (latex . t)
   (python . t)
   (R . t)
   (ruby . t)
   (sh . t)
   (sql . t)
   (sqlite . t)
   )
 )


; Requires ditaa to be installed
(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

(provide 'setup-orgmode)
