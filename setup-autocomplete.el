;; auto-complete
; https://github.com/auto-complete/auto-complete

(require 'auto-complete-config)
; Had to add the files in this directory by hand
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict/")
(ac-config-default)

(setq ac-use-menu-map t)
(setq ac-auto-show-menu 0.2)
(setq ac-ignore-case t)
(setq ac-fuzzy-enable t)
; (setq ac-quick-help-delay 1.0)

(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(defun add-ac-trigger-command (command)
  (if (functionp command)
      (setq ac-trigger-commands (cons command ac-trigger-commands))))
(add-ac-trigger-command 'backward-delete-char-untabify)
(add-ac-trigger-command 'autopair-backspace)

(provide 'setup-autocomplete)
