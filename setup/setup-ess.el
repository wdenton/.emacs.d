;;;;
;;;; Emacs Speaks Statistics (for R)
;;;;

(use-package ess
  :config
  (setq ess-smart-S-assign-key nil) ;; Don't let _ turn into <-
  (setq ess-use-auto-complete t) ;; Auto-completion on.
  (setq ess-use-flymake nil) ;; Don't run flymake on ESS buffers
  (setq ess-help-own-frame 'nil) ;; Make all help buffers go into one frame
  (setq inferior-ess-primary-prompt "ℝ> ") ;; Fancier prompt (see also ~/.Rprofile)
  (setq inferior-S-prompt "[]a-zA-Z0-9.[]*\\(?:[>+.] \\)*ℝ+> ")
  :init
  (add-hook 'ess-R-post-run-hook 'ess-execute-screen-options) ;; Use the full width of the Emacs frame
  (add-hook 'ess-post-run-hook 'ess-execute-screen-options)
  (add-hook 'ess-mode-hook 'indent-guide-mode)   ;; indent-guide ... very nice
  )

;; Use polymode (see elsewhere) for Rmd files
;; (add-to-list 'auto-mode-alist '("\\.Rmd$" . R-mode))

;; Start R in the current directory.  May need to change dirs with setwd() after.
(setq ess-ask-for-ess-directory nil)

;; Show line numbers
;; (add-hook 'ess-mode-hook (lambda () (setq display-line-numbers 'relative)))

;; Display %>% as |, thanks to prettify-symbols-mode
(add-hook 'inferior-ess-mode-hook
	  (lambda ()
	    (push '("%>%" . ?|) prettify-symbols-alist)))

;;
(setq ess-local-process-name "R")

;; I'm not sure what all this does, but it works.
;; Stops comments from flying all the way over to the right, and
;; makes %>% chains indent nicely (if the newline is after the pipe).
(add-hook 'ess-mode-hook
	  (lambda ()
	    (setq ess-indent-level 4)
	    (setq ess-first-continued-statement-offset 2)
	    (setq ess-continued-statement-offset 0)
	    (setq ess-offset-continued 'straight)
	    (setq ess-brace-offset -4)
            (setq ess-expression-offset 4)
            (setq ess-else-offset 0)
            (setq ess-close-brace-offset 0)
            (setq ess-brace-imaginary-offset 0)
            (setq ess-continued-brace-offset 0)
            (setq ess-arg-function-offset 4)
	    (setq ess-arg-function-offset-new-line '(4))
	    ))

;; Be more colourful
(setq ess-R-font-lock-keywords
      (quote
       ((ess-R-fl-keyword:modifiers . t)
        (ess-R-fl-keyword:fun-defs . t)
        (ess-R-fl-keyword:keywords . t)
        (ess-R-fl-keyword:assign-ops . t)
        (ess-R-fl-keyword:constants . t)
        (ess-fl-keyword:fun-calls . t)
        (ess-fl-keyword:numbers . t)
        (ess-fl-keyword:operators . t)
        (ess-fl-keyword:delimiters . t)
        (ess-fl-keyword:=)
	(ess-R-fl-keyword:F&T))))

;; Colourise the comint buffer
(setq ansi-color-for-comint-mode 'filter)

;; Flycheck and lintr
(setq-default flycheck-lintr-linters
              (concat "with_defaults(line_length_linter(120), "
                      "absolute_paths_linter = NULL, "
		      ;; "camel_case_linter = NULL, "
		      ;; "snake_case_linter = NULL, "
		      "commented_code_linter = NULL)"))

;; This next bit is taken from Kieran Healey (http://kieranhealy.org/blog/archives/2009/10/12/make-shift-enter-do-a-lot-in-ess/),
;; who adapted it from http://www.emacswiki.org/emacs/ESSShiftEnter
;; He explains:
;;
;; "Starting with an R file in the buffer, hitting shift-enter
;; vertically splits the window and starts R in the right-side buffer.
;; If R is running and a region is highlighted, shift-enter sends the
;; region over to R to be evaluated. If R is running and no region is
;; highlighted, shift-enter sends the current line over to R.
;; Repeatedly hitting shift-enter in an R file steps through each line
;; (sending it to R), skipping commented lines. The cursor is also
;; moved down to the bottom of the R buffer after each evaluation.
;; Although you can of course use various emacs and ESS keystrokes to
;; do all this (C-x-3, C-c-C-r, etc, etc) it’s convenient to have them
;; bound in a context-sensitive way to one command."

(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (R)
        (set-window-buffer w2 "*R*")
        (set-window-buffer w1 w1name))))
(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))
(add-hook 'ess-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))
(add-hook 'inferior-ess-mode-hook
          '(lambda()
             (local-set-key [C-up] 'comint-previous-input)
             (local-set-key [C-down] 'comint-next-input)))
(add-hook 'Rnw-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))

(provide 'setup-ess)
