; Nice bullets!
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

; Hide the /italics/ and *bold* markers
(setq org-hide-emphasis-markers t)

; Hit return on a link to open it in a browser
(setq org-return-follows-link t)

; Shift and arrow keys to select text works a bit differently in org-mode
(setq org-support-shift-select t)

; Fontify Babel blocks nicely
(setq org-src-fontify-natively t)

; Let's try speed commands.
; "Single keys can be made to execute commands when the cursor is at the beginning of a headline, i.e., before the first star."
(setq org-use-speed-commands t)

; Embed an image with [[file:foo.png]] and then C-c C-x C-v to view
(setq org-display-inline-images t)
; Display images when a file is loaded (I can always toggle them off if I don't want them)
(add-hook 'org-mode-hook (lambda () (org-toggle-inline-images)))

; Use LaTeX spell-check
(add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

; For org-reveal, which makes presentations using reveal.js
; (require 'org-reveal)

; Exporting
(setq org-export-backends (quote (html latex odt)))

(setq org-export-with-smart-quotes t)

; Capturing
(setq org-default-notes-file "~/org/capture.org") ; Change this when I use it for real
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("w" "Work todo" entry (file+headline "~/york/gtd/next-actions.org" "Tasks") "* TODO %?\n %u\n %a")
        ("n" "Note"      entry (file+datetree "~/org/capture.org")                   "* %?\nEntered on %U\n  %i\n %a"))
      )

; Active Babel languages
; See http://orgmode.org/org.html#Languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (ditaa . t)
   (dot . t)
   (latex . t)
   (lilypond . t)
   (python . t)
   (R . t)
   (ruby . t)
   (sh . t)
   (sql . t)
   (sqlite . t)
   )
 )

; Evaluate Babel blocks without asking for confirmation
(setq org-confirm-babel-evaluate nil)

; Requires ditaa to be installed
(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

(provide 'setup-orgmode)
