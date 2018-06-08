;;;;
;;;; Ruby
;;;;

(autoload 'ruby-mode "ruby-mode" "Mode for editing Ruby")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;; I use rbenv, so Emacs needs to know about it
;; https://github.com/senny/rbenv.el
(require 'rbenv)
;; (global-rbenv-mode)
(setq rbenv-show-active-ruby-in-modeline nil)
(setq rbenv-modeline-function 'rbenv--modeline-plain)
(add-hook 'ruby-mode-hook 'global-rbenv-mode)
;; (add-hook 'ruby-mode-hook 'rbenv-use-corresponding)

;; Open up irb with M-x inf-ruby or C-c C-s from a Ruby buffer
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;; Enable auto-completion in inf-ruby
(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'inf-ruby-mode))
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)

;; Avoid ridiculous Ruby indentation.
(setq ruby-deep-indent-paren nil)

;; Show the starting point of a block when I'm at the end.
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; I don't want autopair or smartparens working in Ruby buffers ...
;; (add-hook 'ruby-mode-hook
;; 	  #'(lambda ()
;; 	      (autopair-mode -1))
;; 	  )

;; This seems to fix some problems.  I'm trying using this and not ruby-electric,
;; in the hopes that when I type "  I don't get "\""\"
(require 'smartparens-ruby)

;; "Highlights delimiters such as parentheses, brackets or braces according to their depth."
(add-hook 'ruby-mode-hook #'rainbow-delimiters-mode)

;; "Finds all the URLs in the buffer, highlights them, and turns them into clickable buttons."
;; Use C-c RET to follow a link.
;; TODO Make Org's C-c C-o more general and follow these links too.
(add-hook 'ruby-mode-hook #'goto-address-mode)

;; ... because I use electric mode to do that and more.
;; (require 'ruby-electric)

;; Highlight syntax errors on the fly.
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; Trying out Rubocop (https://github.com/bbatsov/rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)

;; indent-guide ... very nice
(add-hook 'ruby-mode-hook 'indent-guide-mode)

;; Show line numbers
;; (add-hook 'ruby-mode-hook (lambda () (setq display-line-numbers 'relative)))

(provide 'setup-ruby)
