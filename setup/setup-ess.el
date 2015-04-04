;; Emacs Speaks Statistics (for R)

(require 'ess-site)

(setq ess-use-auto-complete t)

;; Make R go to the width of the Emacs frame
(add-hook 'ess-R-post-run-hook 'ess-execute-screen-options)

;; Make knitr files be recognized as R
(add-to-list 'auto-mode-alist '("\\.Rmd$" . R-mode))

; (autoload 'ess-R-object-popup "ess-R-object-popup" "Get info for object at point, and display it in a popup" t)
; (require 'ess-R-object-popup)
(define-key ess-mode-map "\C-c\C-g" 'ess-R-object-popup)

;; Make all help buffers go into one frame
(setq ess-help-own-frame 'nil)

;; Start R in the current directory.  May need to change dirs with setwd() after.
(setq ess-ask-for-ess-directory nil)

;; indent-guide ... very nice
(add-hook 'ess-mode-hook 'indent-guide-mode)

;; Fancy up the prompt (see also ~/.Rprofile)
;; http://www.wisdomandwonder.com/article/9687/i-wasted-time-with-a-custom-prompt-for-r-with-ess
(setq inferior-ess-primary-prompt "ℝ> ")
(setq inferior-S-prompt "[]a-zA-Z0-9.[]*\\(?:[>+.] \\)*ℝ+> ")

;;
(setq ess-local-process-name "R")

;; I use a lot of snake_case variable names, so disable _ turning into <-
(ess-toggle-underscore nil)

;; Had indent problems where tab would force lines way, way over to the right
;; https://github.com/emacs-ess/ESS/issues/99
(add-hook 'ess-mode-hook
	  (lambda ()
	    (setq ess-first-continued-statement-offset 2)
	    (setq ess-continued-statement-offset 0)))

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
