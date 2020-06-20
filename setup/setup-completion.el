;;; setup-completion.el --- Set up completions.

;;; Commentary:

;; Configuration for completions, which I do with company.

;;; Code:

(use-package company
  ;; https://company-mode.github.io/
  :diminish company-mode
  :init
  ;; (global-company-mode)
  (add-hook 'ruby-mode-hook 'company-mode)
  :diminish company-mode
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 1)
  (setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (setq company-tooltip-limit 20)                      ; bigger popup window
  )

(provide 'setup-completion)

;;; setup-completion.el ends here
