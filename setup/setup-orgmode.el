;; Configuration for Org mode (http://orgmode.org/)

;; Try this: https://github.com/abo-abo/org-download/blob/master/org-download.el

;; Common for all Org users
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Files to look in for agenda items
(setq org-agenda-files (quote ("~/york/shared/ar/diary.org" "~/york/shared/projects.org")))

;; Nicer bullets!
;; Possibilities include:  ◉ ○ ✸ ✿ ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶ ► • ★ ▸ or any other amusing Unicode character
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("◉" "►" "•" "•")) ; Default is '("◉" "○" "✸" "✿")

;; org-entities displays \alpha etc. as Unicode characters.
(setq org-pretty-entities t)
;; In 24.4 I can generalize this with prettify-symbols-mode (http://ergoemacs.org/emacs/emacs_pretty_lambda.html)

;; Doesn't handle em and en dashes, though, so let's make that work.
; (setq org-entities-user nil)
; (add-to-list 'org-entities-user '("my-em-mdash" "---" nil "&mdash;" "--" "--" "—"))
; Doesn't work ... and anyway on my screen I'd rather see --- than —

;; Hide the /italics/ and *bold* markers
(setq org-hide-emphasis-markers t)

;; Change the sizes of the titles.
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.1))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

;; I'm trying out variable pitch (proportional) fonts in Org, but I want fixed pitch (monospaced) in source code and tables.
;; Taken from http://yoo2080.wordpress.com/2013/05/30/monospace-font-in-tables-and-source-code-blocks-in-org-mode-proportional-font-in-other-parts/
;; See also http://stackoverflow.com/questions/3758139/variable-pitch-for-org-mode-fixed-pitch-for-tables
;; (defun my-adjoin-to-list-or-symbol (element list-or-symbol)
;;   (let ((list (if (not (listp list-or-symbol))
;;                   (list list-or-symbol)
;;                 list-or-symbol)))
;;     (require 'cl-lib)
;;     (cl-adjoin element list)))

;; (eval-after-load "org"
;;   '(mapc
;;     (lambda (face)
;;       (set-face-attribute
;;        face nil
;;        :inherit
;;        (my-adjoin-to-list-or-symbol
;;         'fixed-pitch
;;         (face-attribute face :inherit))))
;;     (list 'org-code 'org-block 'org-table 'org-block-background)))

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
; (define-key org-mode-map (kbd "C-c C-S-t") 'org-todo-force-notes)

;; Hit return on a link to open it in a browser
(setq org-return-follows-link t)

;; Shift and arrow keys to select text works a bit differently in org-mode
(setq org-support-shift-select t)

;; Make C-a and C-e understand how headings and tags work
;; Seen at http://schenizzle.wordpress.com/2014/03/26/org-mode-ctrl-a-ctrl-e/
(setq org-special-ctrl-a/e t)

;; Something funny with electric-indent-mode in Emacs 25 and indenting stopped in Org.
(setq org-startup-indented t)

;; Fontify Babel blocks nicely
(setq org-src-fontify-natively t)

;; Allow a) b) c) lists
(setq org-list-allow-alphabetical t)

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

;; For org-reveal, which makes presentations using reveal.js
(require 'ox-reveal)

;; Exporting: I will see these export options after C-c C-e
(setq org-export-backends (quote (beamer html latex md odt reveal)))

;; I may need to customize org-html-doctype (default is "xhtml-strict")
; (setq org-html-doctype "html5")

;; Turn ' and " into ‘posh’ “quotes”
(setq org-export-with-smart-quotes t)

;; Define my own link abbreviations
(setq org-link-abbrev-alist
      '(
	("DOI" . "http://dx.doi.org/%s") ;; Thus [[DOI:10.1108/07378831111138189]]
	("WP"  . "https://en.wikipedia.org/wiki/%s") ;; Thus [[WP:Toronto, Ontario]]
	)
      )

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
					; (require 'org-protocol)
					; (setq org-protocol-default-template-key "n")

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
