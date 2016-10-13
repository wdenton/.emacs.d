;;;;
;;;; Configuration for Org mode (http://orgmode.org/)
;;;;

;; Note I made on 16 April 2013:
;; "Started using org-mode ... I could really get into this."

;; Try this: https://github.com/abo-abo/org-download/blob/master/org-download.el

;; I should try org-ref
;; https://github.com/jkitchin/org-ref

;; Common for all Org users
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
;; I never use this.  Trying the binding to do something different.
;; (global-set-key "\C-cb" 'org-iswitchb)

;; Files to look in for agenda items
(setq org-agenda-files (quote ("~/york/shared/projects/projects.org")))

;; Nicer bullets!
;; Possibilities include:  ◉ ○ ✸ ✿ ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶ ► • ★ ▸ or any other amusing Unicode character
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("◉" "○ ""►" "•" "•")) ; Default is '("◉" "○" "✸" "✿")

;; Change the ellipsis that indicates hidden content
;; http://endlessparentheses.com/changing-the-org-mode-ellipsis.html
(setq org-ellipsis " ⬎") ;; ⤵ ↴ ⬎ ⤷
(set-face-attribute 'org-ellipsis nil :underline nil)

;; org-entities displays \alpha etc. as Unicode characters.
(setq org-pretty-entities t)

;; Hide the /italics/ and *bold* markers
(setq org-hide-emphasis-markers t)

;; Change the sizes of the titles.
;; (set-face-attribute 'org-level-1 nil :inherit 'outline-1 :height 1.2)
;; (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :height 1.1)

;; Better colouring of TODO keywords
(setq org-todo-keyword-faces
      (quote (
	      ("TODO" :foreground "SeaGreen" :weight normal)
	      ("WAITING" :foreground "Purple" :weight normal)
	      )))

;; Removed from Org a while back.
;; (defface org-block-background
;;   '((t (:background "#dadada")))
;;   "Face used for the source block background.")

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
;; (setq org-tags-column (- 50 (window-width)))
(setq org-tags-column 120)

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
;; 21 Dec 2015: Doesn't work with new Org export back end
;; (require 'ox-reveal)

;; Exporting: I will see these export options after C-c C-e
(setq org-export-backends (quote (html latex md odt))) ;; beamer reveal

;; I may need to customize org-html-doctype (default is "xhtml-strict")
;; (setq org-html-doctype "html5")

;; Turn ' and " into ‘posh’ “quotes”
(setq org-export-with-smart-quotes t)

;; Use wrap-region
(add-hook 'org-mode-hook 'wrap-region-mode)

;; Since I'm using C-x n to narrow and widen source blocks (see
;; narrow-or-widen-dwim in init.el) I don't need to use C-c ` to enter
;; and leave them, so I can use C-x C-s to save and exit them, which
;; is nice.
;; Taken from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(eval-after-load 'org-src
  '(define-key org-src-mode-map
     "\C-x\C-s" #'org-edit-src-exit))

;; Footnotes. I want them defined nearby, not at the bottom of the
;; document, when I use C-c C-x f.  And I don't want them resorted
;; or adjusted without my saying so.
(setq org-footnote-section nil)
(setq org-footnote-auto-adjust nil)

;; Define my own link abbreviations
(setq org-link-abbrev-alist
      '(
	("DOI" . "http://dx.doi.org/%s")                        ;; Thus [[DOI:10.1108/07378831111138189]]
	("WP"  . "https://en.wikipedia.org/wiki/%s")            ;; Thus [[WP:Toronto, Ontario]]
	("YUL" . "https://www.library.yorku.ca/find/Record/%s") ;; Thus [[YUL:2935857]]
	)
      )

;; Replace a link with just the descriptive text
;; https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
(defun wdenton/org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (let ((remove (list (match-beginning 0) (match-end 0)))
	    (description (if (match-end 3)
			     (org-match-string-no-properties 3)
			   (org-match-string-no-properties 1))))
	(apply 'delete-region remove)
	(insert description))))

;; Hooks for prettify-symbols-mode
;; See also https://pank.eu/blog/pretty-babel-src-blocks.html for some cool stuff
(add-hook 'org-mode-hook
 	  (lambda ()
 	    (push '("<=" . ?≤) prettify-symbols-alist)
 	    (push '(">=" . ?≥) prettify-symbols-alist)
 	    (push '("#+BEGIN_SRC" . ?✎) prettify-symbols-alist)
 	    (push '("#+END_SRC" . ?□) prettify-symbols-alist)
 	    ))

;; Capturing
;; (setq org-default-notes-file "~/org/capture.org") ; Change this when I use it for real
;; (define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(
      	("w" "Work todo" entry (file+headline "~/york/shared/projects/projects.org" "Tasks") "* TODO %?\n %u\n %a")
      	("d" "Work diary" entry (file+datetree "~/york/shared/work-diaries/work-diary.org" "Tasks") "** %?\n %u\n %a")
	("n" "Note"      entry (file+datetree "~/org/capture.org")                   "* %?\nEntered on %U\n  %i\n %a"))
      )

;; Refiling
(setq org-refile-targets '(
			   ("~/york/shared/projects/projects.org" :maxlevel . 1)
			   ;; ("~/york/shared/3dprinting.org" :maxlevel . 1)
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
   (shell . t)
   (sql . t)
   (sqlite . t)
   )
 )

;; Evaluate Babel blocks without asking for confirmation
(setq org-confirm-babel-evaluate nil)

;; Use C-c t to toggle ":eval no|yes" status in source blocks
;; https://emacs.stackexchange.com/questions/13869/how-to-toggle-org-mode-source-code-block-eval-no-status
(defun org-toggle-src-eval-no ()
  "Will toggle ':eval no' on the src block begin line"
  (defun in-src-block-p ()
    "Returns t when the point is inside a source code block"
    (string= "src" (org-in-block-p '("src"))))
  (defun beginning-src ()
    "Find the beginning of the src block"
    (let ((case-fold-search t)) (search-backward "#+BEGIN_SRC")))
  (defun toggle-eval-no ()
    "Handles the toggling of ' :eval no'"
    (save-excursion
      (end-of-line)
      (let ((case-fold-search t)) (search-backward "#+BEGIN_SRC")
	   (if (search-forward " :eval no" (line-end-position) "f")
	       (replace-match "")
	     (insert " :eval no")
	     ))))
  (if (in-src-block-p) (toggle-eval-no)))
(defun add-org-toggle-src-key ()
  (local-set-key (kbd "C-c t") (lambda () (interactive) (org-toggle-src-eval-no))))
(add-hook 'org-mode-hook 'add-org-toggle-src-key)

;; In 25 Org started opening exported PDFs in docview, but I prefer
;; seeing them externally.
(delete '("\\.pdf\\'" . default) org-file-apps)
(add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))

;; Requires ditaa to be installed
(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

;; Use C-< to wrap a block of text in a block, like < would create (for SRC or QUOTE)
;; From http://pragmaticemacs.com/emacs/wrap-text-in-an-org-mode-block/
(defun wdenton/org-begin-template ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("q" . "QUOTE")
                      ("v" . "VERSE")
                      ("c" . "CENTER")
                      ("l" . "LaTeX")
                      ("h" . "HTML")
                      ("a" . "ASCII")))
           (key
	    (key-description
	     (vector
	      (read-key
	       (concat
		(propertize "Template type: " 'face 'minibuffer-prompt)
		(mapconcat (lambda (choice)
			     (concat
			      (propertize (car choice) 'face 'font-lock-type-face)
			      ": "
			      (cdr choice)))
			   choices
			   ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+END_" choice "\n")
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "#+END_" choice))))))))))
(define-key org-mode-map (kbd "C-<") 'wdenton/org-begin-template)

;; Stop ispell from looking where it shouldn't.
;; http://endlessparentheses.com/ispell-and-org-mode.html
(defun wdenton/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE ". "#\\+END_EXAMPLE"))
  )
(add-hook 'org-mode-hook #'wdenton/org-ispell)

;; Integrate RefTeX
;; From http://orgmode.org/worg/org-faq.html#using-reftex-in-org-mode
;; Use these lines to generate the bib (Org will recognize them as LaTeX commands):
;; \bibliographystyle{plain}
;; \bibliography{BIB-NAME}
;;
;; With this setup, C-c ) will invoke reftex-citation which will insert a reference in the usual way.

;; (defun org-mode-reftex-setup ()
;;   (load-library "reftex")
;;   (and (buffer-file-name)
;;        (file-exists-p (buffer-file-name))
;;        (reftex-parse-all))
;;   (define-key org-mode-map (kbd "C-c )") 'reftex-citation))
;; (add-hook 'org-mode-hook 'org-mode-reftex-setup)

(provide 'setup-orgmode)
