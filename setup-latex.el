;; Everything related to LaTeX and Auctex

; Except auto-completion in Auctex is turned on in 
; setup-autocomplete.el

; Good reading:
; http://tex.stackexchange.com/questions/50827/a-simpletons-guide-to-tex-workflow-with-emacs/50919#50919
; http://tex.stackexchange.com/questions/52179/what-is-your-favorite-emacs-and-or-auctex-command-trick 

(defun turn-on-outline-minor-mode () (outline-minor-mode 1))
(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-visual-line-mode)

; Turn on spell-checking in LaTeX
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

; auto-complete-auctex (see setup-autocomplete.el for the rest of the auto-complete setup)
(require 'auto-complete-auctex)
(add-hook 'LaTeX-mode-hook 'auto-complete-auctex)

; (setq latex-run-command "pdflatex")
; Use pdflatex to make PDFs
; For some reason this value isn't respected and I had to set
; it through Custom. Don't know why.
(setq TeX-PDF-mode t)

(setq biblatex-dialect "biblatex")

; Zotero integration! (https://github.com/vitoshka/zotelo)
(require 'zotelo)
(add-hook 'TeX-mode-hook 'zotelo-minor-mode)

; Automatically activate TeX-fold-mode.
; C-c C-o C-b is necessary to hide everything (or see LaTeX | Show/Hide)
(add-hook 'TeX-mode-hook (lambda () (TeX-fold-mode 1)))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

; So that RefTeX also recognizes \addbibresource. Note that you
; can't use $HOME in path for \addbibresource but that "~" works.
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

; Remember:
; When editing tables:
; M-x align-current

; From http://tex.stackexchange.com/questions/50827/a-simpletons-guide-to-tex-workflow-with-emacs/50919#50919
(eval-after-load 'reftex-vars
  '(progn
     ;; (also some other reftex-related customizations)
     (setq reftex-cite-format
           '((?\C-m . "\\cite[]{%l}")
             (?f . "\\footcite[][]{%l}")
             (?t . "\\textcite[]{%l}")
             (?p . "\\parencite[]{%l}")
             (?o . "\\citepr[]{%l}")
             (?n . "\\nocite{%l}")))))

; Fontification (remove unnecessary entries as you notice them)
; http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00236.html
; http://www.gnu.org/software/auctex/manual/auctex/Fontification-of-macros.html
(setq font-latex-match-reference-keywords
      '(
        ;; biblatex
        ("printbibliography" "[{")
        ("addbibresource" "[{")
        ;; Standard commands
        ;; ("cite" "[{")
        ("Cite" "[{")
        ("parencite" "[{")
        ("Parencite" "[{")
        ("footcite" "[{")
        ("footcitetext" "[{")
        ;; ;; Style-specific commands
        ("textcite" "[{")
        ("Textcite" "[{")
        ("smartcite" "[{")
        ("Smartcite" "[{")
        ("cite*" "[{")
        ("parencite*" "[{")
        ("supercite" "[{")
        ; Qualified citation lists
        ("cites" "[{")
        ("Cites" "[{")
        ("parencites" "[{")
        ("Parencites" "[{")
        ("footcites" "[{")
        ("footcitetexts" "[{")
        ("smartcites" "[{")
        ("Smartcites" "[{")
        ("textcites" "[{")
        ("Textcites" "[{")
        ("supercites" "[{")
        ;; Style-independent commands
        ("autocite" "[{")
        ("Autocite" "[{")
        ("autocite*" "[{")
        ("Autocite*" "[{")
        ("autocites" "[{")
        ("Autocites" "[{")
        ;; Text commands
        ("citeauthor" "[{")
        ("Citeauthor" "[{")
        ("citetitle" "[{")
        ("citetitle*" "[{")
        ("citeyear" "[{")
        ("citedate" "[{")
        ("citeurl" "[{")
        ;; Special commands
        ("fullcite" "[{")))

(setq font-latex-match-textual-keywords
      '(
        ;; biblatex brackets
        ("parentext" "{")
        ("brackettext" "{")
        ("hybridblockquote" "[{")
        ;; Auxiliary Commands
        ("textelp" "{")
        ("textelp*" "{")
        ("textins" "{")
        ("textins*" "{")
        ;; supcaption
        ("subcaption" "[{")))

(setq font-latex-match-variable-keywords
      '(
        ;; amsmath
        ("numberwithin" "{")
        ;; enumitem
        ("setlist" "[{")
        ("setlist*" "[{")
        ("newlist" "{")
        ("renewlist" "{")
        ("setlistdepth" "{")
        ("restartlist" "{")))

(provide 'setup-latex)

