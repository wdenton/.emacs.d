;; Ruby mode

(autoload 'ruby-mode "ruby-mode" "Mode for editing Ruby")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

; Open up irb with M-x inf-ruby or C-c C-s from a Ruby buffer
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

; Avoid ridiculous Ruby indentation.
(setq ruby-deep-indent-paren nil)

; Show the starting point of a block when I'm at the end.
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

; I don't want autopair or smartparens working in Ruby buffers ...
(add-hook 'ruby-mode-hook
	  #'(lambda ()
	      (autopair-mode -1))
	  )
;(add-hook 'ruby-mode-hook
;	  #'(lambda ()
;	      (smart-parens-mode -1))
;	  )

; This seems to fix some problems.  I'm trying using this and not ruby-electric,
; in the hopes that when I type "  I don't get "\""\"
(require 'smartparens-ruby)

; ... because I use electric mode to do that and more.
; (require 'ruby-electric)

; Highlight syntax errors on the fly.
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

; Trying out Rubocop (https://github.com/bbatsov/rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)

; Make Emacs know about RVM (https://github.com/senny/rvm.el)
(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

; Rinari (for Ruby on Rails)
; (require 'rinari)

(provide 'setup-ruby)
