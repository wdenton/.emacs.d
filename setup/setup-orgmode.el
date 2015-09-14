;;;;
;;;; Configuration for Org mode (http://orgmode.org/)
;;;;

;; Try this: https://github.com/abo-abo/org-download/blob/master/org-download.el

;; Common for all Org users
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
;; I never use this.  Trying the binding to do something different.
;; (global-set-key "\C-cb" 'org-iswitchb)

;; Files to look in for agenda items
(setq org-agenda-files (quote ("~/york/shared/ar/ar-diary.org" "~/york/shared/projects/projects.org")))

;; Nicer bullets!
;; Possibilities include:  ◉ ○ ✸ ✿ ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶ ► • ★ ▸ or any other amusing Unicode character
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("◉" "○ ""►" "•" "•")) ; Default is '("◉" "○" "✸" "✿")

;; org-entities displays \alpha etc. as Unicode characters.
(setq org-pretty-entities t)

;; Hide the /italics/ and *bold* markers
(setq org-hide-emphasis-markers t)

;; Change the sizes of the titles.
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 )

;; Better colouring of TODO keywords
(setq org-todo-keyword-faces
      (quote (
	      ("TODO" :foreground "SeaGreen" :weight normal)
	      ("WAITING" :foreground "Purple" :weight normal)
	      )))

(defface org-block-background
  '((t (:background "#dadada")))
  "Face used for the source block background.")

;; I asked and someone answered on the beta Emacs SE.
;; https://emacs.stackexchange.com/questions/90/how-to-sometimes-but-not-always-add-a-note-to-an-org-todo-state-change
;; This lets me force a note for any state change in TODO workflow.
;; Use C-c C-T (capital T) to make Org ask me for a note, even if the normal workflow doesn't require it.
(defun org-todo-force-notes ()
  (interactive)
  (let ((org-todo-log-states
         (mapcar (lambda (state)
                   (list state 'note 'time))
                 (apply 'append org-todo-sets))))
    (call-interactively 'org-todo)))
;; (define-key org-mode-map (kbd "C-c C-S-t") 'org-todo-force-notes)

;; Hit return on a link to open it in a browser
(setq org-return-follows-link t)

;; Shift and arrow keys to select text works a bit differently in org-mode
(setq org-support-shift-select t)

;; Make C-a and C-e understand how headings and tags work
;; Seen at http://schenizzle.wordpress.com/2014/03/26/org-mode-ctrl-a-ctrl-e/
(setq org-special-ctrl-a/e t)

;; Visually indent everything nicely, but leave the raw file left-aligned
(setq org-startup-indented t)

;; Fontify Babel blocks nicely
(setq org-src-fontify-natively t)

;; Allow a) b) c) lists
(setq org-list-allow-alphabetical t)

;; Right-align tags to an indent from the right margin
(setq org-tags-column (- 50 (window-width)))

;; "Single keys can be made to execute commands when the cursor is at
;; the beginning of a headline, i.e., before the first star."
(setq org-use-speed-commands t)

;; Embed an image with [[file:foo.png]] and then C-c C-x C-v to view
(setq org-display-inline-images t)

;; Automatically refresh inline images that are generated from Babel blocks
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Display images when a file is loaded (I can always toggle them off if I don't want them)
(add-hook 'org-mode-hook (lambda () (org-toggle-inline-images)))

;; imenu integration
(add-hook 'org-mode-hook
	  (lambda () (imenu-add-to-menubar "Imenu")))

;; Use LaTeX spell-check
(add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))

;; Ispell should ignore some things in Org files
;; http://endlessparentheses.com/ispell-and-org-mode.html
(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
(add-hook 'org-mode-hook #'endless/org-ispell)

;; Preview LaTeX equations in buffers by showing images (C-c C-x C-l)
;; Details: http://orgmode.org/worg/org-tutorials/org-latex-preview.html
(setq org-latex-create-formula-image-program 'imagemagick)

;; For org-reveal, which makes presentations using reveal.js
(require 'ox-reveal)

;; Exporting: I will see these export options after C-c C-e
(setq org-export-backends (quote (beamer html latex md odt reveal)))

;; In 25 Org started opening exported PDFs in docview, but I prefer
;; seeing them externally.
(delete '("\\.pdf\\'" . default) org-file-apps)
(add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))

;; I may need to customize org-html-doctype (default is "xhtml-strict")
;; (setq org-html-doctype "html5")

;; Turn ' and " into ‘posh’ “quotes”
(setq org-export-with-smart-quotes t)

;; Footnotes. I want them defined nearby, not at the bottom of the
;; document, when I use C-c C-x f.  And I don't want them resorted
;; or adjusted without my saying so.
(setq org-footnote-section nil)
(setq org-footnote-auto-adjust nil)

;; Define my own link abbreviations
(setq org-link-abbrev-alist
      '(
	("DOI" . "http://dx.doi.org/%s") ;; Thus [[DOI:10.1108/07378831111138189]]
	("WP"  . "https://en.wikipedia.org/wiki/%s") ;; Thus [[WP:Toronto, Ontario]]
	)
      )

;; Hooks for prettify-symbols-mode
(add-hook 'org-mode-hook
	  (lambda ()
	    (push '("<=" . ?≤) prettify-symbols-alist)
	    (push '(">=" . ?≥) prettify-symbols-alist)
	    ))

;; Capturing
(setq org-default-notes-file "~/org/capture.org") ; Change this when I use it for real
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(
	("w" "Work todo" entry (file+headline "~/york/shared/projects.org" "Tasks") "* TODO %?\n %u\n %a")
	("d" "Work diary" entry (file+datetree "~/york/shared/workdiary.org" "Tasks") "** %?\n %u\n %a")
        ("n" "Note"      entry (file+datetree "~/org/capture.org")                   "* %?\nEntered on %U\n  %i\n %a"))
      )

;; Refiling
(setq org-refile-targets '(
			   ("~/york/shared/projects.org" :maxlevel . 1)
			   ("~/york/shared/3dprinting.org" :maxlevel . 1)
			   ))

;; org-protocol lets me send URLs from Firefox (and more, but that's all I'm doing)
;; See https://stackoverflow.com/questions/7464951/how-to-make-org-protocol-work
;; (require 'org-protocol)
;; '(setq org-protocol-default-template-key "n")

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
