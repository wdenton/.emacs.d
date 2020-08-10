;;; setup-orgmode.el --- Set up Org.

;;; Commentary:

;; Note I made on 16 April 2013:
;; "Started using org-mode ... I could really get into this."

;; I should try org-ref
;; https://github.com/jkitchin/org-ref

;;; Code:

(use-package org
  ;; Use Org's current development branch, pulled down with git
  ;; (http://orgmode.org/org.html#Installation)
  :load-path "/usr/local/src/org-mode/lisp"

  :config
  (setq org-fontify-whole-heading-line t)
  (global-set-key "\C-cl" 'org-store-link)

  ;; Show column headings even when off the top of the screen.
  (setq org-table-header-line-p nil)
  ;; (diminish 'org-table-header-line-mode)

  ;; org-entities displays \alpha etc. as Unicode characters.
  (setq org-pretty-entities t)

  ;; Hide the /italics/ and *bold* markers
  (setq org-hide-emphasis-markers t)

  ;; Better colouring of TODO keywords
  (setq org-todo-keyword-faces
	(quote (
		("TODO" :foreground "SeaGreen" :weight normal)
		("WAITING" :foreground "Purple" :weight normal)
		)))

  (set-face-attribute 'org-link nil :foreground "Steel Blue")

  ;; Make completed items in a checkbox list less noticeable
  ;; https://fuco1.github.io/2017-05-25-Fontify-done-checkbox-items-in-org-mode.html
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
   'append)

  ;; Hit return on a link to open it in a browser
  (setq org-return-follows-link t)

  ;; Shift and arrow keys to select text works a bit differently in org-mode
  (setq org-support-shift-select t)

  ;; Make C-a and C-e understand how headings and tags work
  ;; Seen at http://schenizzle.wordpress.com/2014/03/26/org-mode-ctrl-a-ctrl-e/
  (setq org-special-ctrl-a/e t)

  ;; Visually indent everything nicely, but leave the raw file left-aligned
  (setq org-startup-indented t)

  ;; Never show blank lines in condensed view
  (setq org-cycle-separator-lines 0)

  ;; Fontify Babel blocks nicely
  (setq org-src-fontify-natively t)

  ;; Preserve indentation when tangling source blocks (important for makefiles)
  (setq org-src-preserve-indentation t)

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

  ;; Show on startup
  (setq org-startup-with-inline-images t)

  ;; How to rearrange things when I edit a source block
  ;; default is regorganize-frame
  (setq org-src-window-setup 'current-window)

  ;; Automatically refresh inline images that are generated from Babel blocks
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  ;; Display images when a file is loaded (I can always toggle them off if I don't want them)
  (add-hook 'org-mode-hook (lambda () (org-toggle-inline-images)))

  ;; Use LaTeX spell-check
  (add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))

  ;; Use flyspell to check spelling as I go
  (add-hook 'org-mode-hook 'turn-on-flyspell)

  ;; Use C-c d to close all the open drawers in a file
  (defun add-org-close-all-drawers-key ()
    (local-set-key (kbd "C-c d") (lambda () (interactive) (org-cycle-hide-drawers 'all))))
  (add-hook 'org-mode-hook 'add-org-close-all-drawers-key)

  ;; Show line numbers (trying this out)
  ;; (add-hook 'org-mode-hook (lambda () (setq display-line-numbers t)))

  ;; imenu integration
  ;; (add-hook 'org-mode-hook
  ;; (lambda () (imenu-add-to-menubar "Imenu")))

  ;; Preview LaTeX equations in buffers by showing images (C-c C-x C-l)
  ;; Details: http://orgmode.org/worg/org-tutorials/org-latex-preview.html
  ;;  (setq org-preview-latex-default-process 'imagemagick)

  ;; Highlight inline LaTeX
  (setq org-highlight-latex-and-related '(latex))

  ;; Exporting: I will see these export options after C-c C-e
  (setq org-export-backends (quote (html latex md odt))) ;; beamer reveal

  ;; I may need to customize org-html-doctype (default is "xhtml-strict")
  ;; (setq org-html-doctype "html5")

  ;; Turn ' and " into ‘posh’ “quotes”
  (setq org-export-with-smart-quotes t)

  ;; Date format on exports
  ;; (setq org-export-date-timestamp-format "%d %m %Y")

  ;; Use wrap-region
  (add-hook 'org-mode-hook 'wrap-region-mode)

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

  ;; Hooks for prettify-symbols-mode
  ;; See also https://pank.eu/blog/pretty-babel-src-blocks.html for some cool stuff
  ;; And https://github.com/zzamboni/dot-emacs/blob/master/init.org#source-code-blocks
  ;; some stuff I tried out but decided was a bit too much for me.
  (add-hook 'org-mode-hook
 	    (lambda ()
 	      (push '("<=" . ?≤) prettify-symbols-alist)
 	      (push '(">=" . ?≥) prettify-symbols-alist)
 	      (push '("#+BEGIN_SRC" . ?⎡) prettify-symbols-alist) ;;  ⎡ ➤ 🖝 ➟ ➤ ✎
 	      (push '("#+END_SRC" . ?⎣) prettify-symbols-alist) ;; ⎣ ✐
 	      (push '("#+begin_src" . ?⎡) prettify-symbols-alist)
 	      (push '("#+end_src" . ?⎣) prettify-symbols-alist)
 	      (push '("#+BEGIN_QUOTE" . ?❝) prettify-symbols-alist)
 	      (push '("#+END_QUOTE" . ?❞) prettify-symbols-alist)
 	      (push '("#+begin_quote" . ?❝) prettify-symbols-alist)
 	      (push '("#+end_quote" . ?❞) prettify-symbols-alist)
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
			     ;; ("~/york/shared/reports/annual/2017-annual-report/denton-2016-2017-annual-report.org" :maxlevel . 2)
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

  ;; Source code block appearance
  (set-face-attribute 'org-block-begin-line nil :underline nil)
  (set-face-attribute 'org-block-end-line nil :overline nil)

  ;; Evaluate Babel blocks without asking for confirmation
  (setq org-confirm-babel-evaluate nil)

  ;; In 25 Org started opening exported PDFs in docview, but I prefer
  ;; seeing them externally.
  (delete '("\\.pdf\\'" . default) org-file-apps)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))

  ;; For this to work, ditaa must be installed
  (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

  ;; Change the ellipsis that indicates hidden content
  ;; http://endlessparentheses.com/changing-the-org-mode-ellipsis.html
  (setq org-ellipsis " ⬎") ;; ⤵ ↴ ⬎ ⤷
  (set-face-attribute 'org-ellipsis nil :underline nil)

  ;; Make LOGBOOK and such fainter.  Default bold is too loud.
  (face-spec-set 'org-drawer '((t (:foreground "dim gray" :weight normal))))

  ;; (face-spec-set 'org-level-1 '((t (:height 1.05))))
  ;; (face-spec-set 'org-level-2 '((t (:height 1.05))))
  ;; (face-spec-set 'org-level-3 '((t (:height 1.0))))

  ;; Clocking
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  )

;;;;
;;;; org-bullets (https://github.com/sabof/org-bullets)
;;;;

(use-package org-bullets
  ;; Possibilities:  ◉ ○ ✸ ✿ ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶ ► • ★ ▸ or any other Unicode character
  ;; Default is '("◉" "○" "✸" "✿")
  ;; I've used ("◉" "○ ""►" "•" "•"))
  :after org
  :config
  (setq org-bullets-bullet-list '("⊢" "⋮" "⋱" "⋱" "⋱"))
  :hook
  (org-mode . (lambda () (org-bullets-mode 1)))
  ;;  :init
  ;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;; I asked and someone answered on the beta Emacs SE.
;; https://emacs.stackexchange.com/questions/90/how-to-sometimes-but-not-always-add-a-note-to-an-org-todo-state-change
;; This lets me force a note for any state change in TODO workflow.
;; Use C-c C-T (capital T) to make Org ask me for a note, even if the normal workflow doesn't require it.
;; (defun org-todo-force-notes ()
;;   (interactive)
;;   (let ((org-todo-log-states
;;          (mapcar (lambda (state)
;;                    (list state 'note 'time))
;;                  (apply 'append org-todo-sets))))
;;     (call-interactively 'org-todo)))
;; (define-key org-mode-map (kbd "C-c C-S-t") 'org-todo-force-notes)

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

;; Since I'm using C-x n to narrow and widen source blocks (see
;; narrow-or-widen-dwim in init.el) I don't need to use C-c ` to enter
;; and leave them, so I can use C-x C-s to save and exit them, which
;; is nice.
;; Taken from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(eval-after-load 'org-src
  '(define-key org-src-mode-map
     "\C-x\C-s" #'org-edit-src-exit))

;; Replace a link with just the descriptive text.
;; https://emacs.stackexchange.com/a/49068/145
(defun wdenton/org-link-delete-link ()
  "Remove the link from an Org link at point and keep only the description."
  (interactive)
  (let ((elem (org-element-context)))
    (if (eq (car elem) 'link)
        (let* ((content-begin (org-element-property :contents-begin elem))
               (content-end  (org-element-property :contents-end elem))
               (link-begin (org-element-property :begin elem))
               (link-end (org-element-property :end elem)))
          (if (and content-begin content-end)
              (let ((content (buffer-substring-no-properties content-begin content-end)))
                (delete-region link-begin link-end)
                (insert content)))))))

;; Use C-c t to toggle ":eval no|yes" status in source blocks
;; https://emacs.stackexchange.com/questions/13869/how-to-toggle-org-mode-source-code-block-eval-no-status
(defun org-toggle-src-eval-no ()
  "Toggle ':eval no' on the src block begin line."
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

;; Stop ispell from looking where it shouldn't.
;; http://endlessparentheses.com/ispell-and-org-mode.html
(defun wdenton/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+begin_src" . "^#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE ". "#\\+END_EXAMPLE"))
  )
(add-hook 'org-mode-hook #'wdenton/org-ispell)

;; So I can use Memoir
;; (with-eval-after-load 'ox-latex
;;   (add-to-list 'org-latex-classes
;;                '("memoir"
;;                  "\\documentclass{memoir}"
;;                  ("\\book{%s}" . "\\book*{%s}")
;;                  ("\\part{%s}" . "\\part*{%s}")
;;                  ("\\chapter{%s}" . "\\chapter*{%s}")
;;                  ("\\section{%s}" . "\\section*{%s}")
;;                  ("\\subsection{%s}" . "\\subsection*{%s}")
;;                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
;;   (add-to-list 'org-latex-classes
;;                '("memoir-chapter+"
;;                  "\\documentclass{memoir}"
;;                  ("\\chapter{%s}" . "\\chapter*{%s}")
;;                  ("\\section{%s}" . "\\section*{%s}")
;;                  ("\\subsection{%s}" . "\\subsection*{%s}")
;;                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
;;   )

(provide 'setup-orgmode)

;;; setup-orgmode.el ends here
