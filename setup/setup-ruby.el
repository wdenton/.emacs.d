;;;;
;;;; Ruby
;;;;

;; I used ruby-block once, but seem to have stopped.
;; https://github.com/juszczakn/ruby-block

(autoload 'ruby-mode "ruby-mode" "Mode for editing Ruby")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;; I use rbenv, so Emacs needs to know about it
;; https://github.com/senny/rbenv.el
(use-package rbenv
  :hook (ruby-mode . global-rbenv-mode)
  :config
  (setq rbenv-show-active-ruby-in-modeline nil)
  (setq rbenv-modeline-function 'rbenv--modeline-plain)
  (rbenv-use-global)
  )

;; Open up irb with M-x inf-ruby or C-c C-s from a Ruby buffer
(use-package inf-ruby
  :hook (ruby-mode . inf-ruby-minor-mode)
  :config
  (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
  (add-to-list 'inf-ruby-implementations' ("pry". "pry"))
  (setq inf-ruby-default-implementation "pry")
  )

;; Avoid ridiculous Ruby indentation.
(setq ruby-deep-indent-paren nil)

;; Don't put the UTF-8 encoding comment at the top
(setq ruby-insert-encoding-magic-comment nil)

;; Parentheses.
(require 'smartparens-ruby)

;; "Finds all the URLs in the buffer, highlights them, and turns them into clickable buttons."
;; Use C-c RET to follow a link.
;; TODO Make Org's C-c C-o more general and follow these links too.
(add-hook 'ruby-mode-hook #'goto-address-mode)

;; Rubocop for pointing out errors (https://github.com/bbatsov/rubocop)
(use-package rubocop
  :diminish rubocop-mode
  :hook ruby-mode
  )

;; Robe for documentation lookup, etc.
;; https://github.com/dgutov/robe
;; Requires gems pry and pry-doc
;; (use-package robe
;;   :diminish robe-mode
;;   :hook ruby-mode
;;   :config
;;   (eval-after-load 'company '(push 'company-robe company-backends))
;;   )

;; indent-guide ... very nice
(add-hook 'ruby-mode-hook 'indent-guide-mode)

;; Show line numbers
;; (add-hook 'ruby-mode-hook (lambda () (setq display-line-numbers 'relative)))

(provide 'setup-ruby)
;;; setup-ruby.el ends here
