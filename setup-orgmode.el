;; Configuration for Org mode (http://orgmode.org/)

;; Try this: https://github.com/abo-abo/org-download/blob/master/org-download.el

;; Common for all Org users
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Files to look in for agenda items
(setq org-agenda-files (quote ("~/york/gtd/ar/diary.org")))

;; Nice bullets!
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Hide the /italics/ and *bold* markers
(setq org-hide-emphasis-markers t)

;; Hit return on a link to open it in a browser
(setq org-return-follows-link t)

;; Shift and arrow keys to select text works a bit differently in org-mode
(setq org-support-shift-select t)

;; Make C-a and C-e understand how headings and tags work
;; Seen at http://schenizzle.wordpress.com/2014/03/26/org-mode-ctrl-a-ctrl-e/
(setq org-special-ctrl-a/e t)

;; Fontify Babel blocks nicely
(setq org-src-fontify-natively t)

;; Let's try speed commands.
;; "Single keys can be made to execute commands when the cursor is at the beginning of a headline, i.e., before the first star."
(setq org-use-speed-commands t)

;; Embed an image with [[file:foo.png]] and then C-c C-x C-v to view
(setq org-display-inline-images t)

;; Automatically refresh inline images that are generated from Babel blocks
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Display images when a file is loaded (I can always toggle them off if I don't want them)
(add-hook 'org-mode-hook (lambda () (org-toggle-inline-images)))

;; Use LaTeX spell-check
(add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))

;; Preview LaTeX equations in buffers by showing images (C-c C-x C-l)
;; Details: http://orgmode.org/worg/org-tutorials/org-latex-preview.html
(setq org-latex-create-formula-image-program 'imagemagick)

; For org-reveal, which makes presentations using reveal.js
; (require 'org-reveal)

;; Exporting: I will see these export options after C-c C-e
(setq org-export-backends (quote (beamer html latex md odt reveal)))

;; I may need to customize org-html-doctype (default is "xhtml-strict")
; (setq org-html-doctype "html5")

;; Turn ' and " into ‘posh’ “quotes”
(setq org-export-with-smart-quotes t)

;; Capturing
(setq org-default-notes-file "~/org/capture.org") ; Change this when I use it for real
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("w" "Work todo" entry (file+headline "~/york/gtd/next-actions.org" "Tasks") "* TODO %?\n %u\n %a")
        ("n" "Note"      entry (file+datetree "~/org/capture.org")                   "* %?\nEntered on %U\n  %i\n %a"))
      )

;; Active Babel languages
;; See http://orgmode.org/org.html#Languages
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

;; Evaluate Babel blocks without asking for confirmation
(setq org-confirm-babel-evaluate nil)

;; Requires ditaa to be installed
(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

;; Make ispell skip source blocks
(add-to-list 'ispell-skip-region-alist '("#\\+begin_src". "#\\+end_src"))

;; Org can read RSS feeds (http://orgmode.org/org.html#RSS-Feeds)
;; "You could use this to make a task out of each new podcast in a
;; podcast feed. Or you could use a phone-based note-creating service
;; on the web to import tasks into Org."
;; I don't have any use for this right now, but perhaps some day.
(setq org-feed-alist
      '(
	("Miskatonic" "http://www.miskatonic.org/feed" "~/org/feeds.org" "Miskatonic")
	("Mastering Emacs" "http://www.masteringemacs.org/feed" "~/org/feeds.org" "Mastering Emacs")
	))

; Integrate RefTeX
; From http://orgmode.org/worg/org-faq.html#using-reftex-in-org-mode
; Use these lines to generate the bib (Org will recognize them as LaTeX commands):
; \bibliographystyle{plain}
; \bibliography{BIB-NAME}
;
; With this setup, C-c ) will invoke reftex-citation which will insert a reference in the usual way.

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation))
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(provide 'setup-orgmode)
