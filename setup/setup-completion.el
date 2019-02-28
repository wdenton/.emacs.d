;;;;
;;;; completion
;;;;

;; company
;; https://company-mode.github.io/

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay .3)
  (setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (setq company-tooltip-limit 20)                      ; bigger popup window
  )

;; autocomplete
;; https://github.com/auto-complete/auto-complete

;; (use-package auto-complete
;;   :ensure t
;;   :diminish auto-complete-mode
;;   :config
;;   (global-auto-complete-mode t)
;;   (define-key ac-menu-map "\C-n" 'ac-next)
;;   (define-key ac-menu-map "\C-p" 'ac-previous)
;;   (setq ac-use-menu-map t)
;;   (setq ac-auto-show-menu 1) ;; 0.2
;;   (setq ac-ignore-case t)
;;   (setq ac-fuzzy-enable t)
;;   ;; (setq ac-quick-help-delay 1.0)
;;   (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict/")
;;   (ac-config-default)
;;   )

(provide 'setup-completion)
