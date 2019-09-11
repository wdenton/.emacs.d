;;;;
;;;; LaTeX and AucTeX
;;;;

;; Except! auto-completion in Auctex is turned on in setup-autocomplete.el

;; Good reading:
;; http://tex.stackexchange.com/questions/50827/a-simpletons-guide-to-tex-workflow-with-emacs/50919#50919
;; http://tex.stackexchange.com/questions/52179/what-is-your-favorite-emacs-and-or-auctex-command-trick

;; Simplify everything below
;; Try these to simplify highlighting
;; (setq font-latex-fontify-script nil)
;; (setq font-latex-fontify-sectioning 'color)

;; (defun turn-on-outline-minor-mode () (outline-minor-mode 1))
;; (add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)

;;; Code:

(add-hook 'LaTeX-mode-hook 'turn-on-visual-line-mode)

;; Turn on spell-checking in LaTeX
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; ;; auto-complete-auctex (see setup-autocomplete.el for the rest of the auto-complete setup)
;; ;; (require 'auto-complete-auctex)
;; ;; (add-hook 'LaTeX-mode-hook 'auto-complete-auctex)
;; (use-package auto-complete-auctex
;;   :config
;;   (add-hook 'LaTeX-mode-hook 'auto-complete-auctex)
;;   )

;; (setq latex-run-command "pdflatex")
;; Use pdflatex to make PDFs
;; For some reason this value isn't respected and I had to set
;; it through Custom. Don't know why.
(setq TeX-PDF-mode t)

(setq biblatex-dialect "biblatex")

;; Zotero integration! (https://github.com/vitoshka/zotelo)
;; Requires MozRepl addon in Firefox
;; (require 'zotelo)
;; (add-hook 'TeX-mode-hook 'zotelo-minor-mode)

;; Automatically activate TeX-fold-mode.
;; C-c C-o C-b is necessary to hide everything (or see LaTeX | Show/Hide)
(add-hook 'TeX-mode-hook (lambda () (TeX-fold-mode 1)))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Use wrap-region
(add-hook 'latex-mode-hook 'wrap-region-mode)

;; Indent lists by 2 (default is -2)
(setq LaTeX-item-indent 0)

;; So that RefTeX also recognizes \addbibresource. Note that you
;; can't use $HOME in path for \addbibresource but that "~" works.
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

;; Remember:
;; When editing tables:
;; M-x align-current

;; From http://tex.stackexchange.com/questions/50827/a-simpletons-guide-to-tex-workflow-with-emacs/50919#50919
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

(provide 'setup-latex)
