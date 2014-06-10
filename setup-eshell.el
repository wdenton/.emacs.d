;; eshell configurations

;; For much more see
;; http://www.masteringemacs.org/articles/2010/12/13/complete-guide-mastering-eshell/

(require 'em-smart)

(setq eshell-aliases-file "~/.emacs.d/eshell/aliases"
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"
      esehll-review-quick-commands t
      eshell-smart-space-goes-to-end t
      eshell-where-to-jump 'begin
      )

(add-hook 'eshell-mode-hook '(lambda ()
                               (define-key eshell-mode-map (kbd "C-p") 'eshell-previous-input)
                               (define-key eshell-mode-map (kbd "C-n") 'eshell-next-input)))

(setq eshell-prompt-function
  (lambda ()
    (concat
     "┌─["
     (format-time-string "%I:%M %p" (current-time))
     "]─["
     (user-login-name)
     "@"
     (system-name)
     "]\n└─> "
     (eshell/pwd)
     (if (= (user-uid) 0) " # " " $ ")
     )
    )
  )

(provide 'setup-eshell)
