;; Ruby mode

(autoload 'ruby-mode "ruby-mode" "Mode for editing Ruby")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

; Open up irb with M-x inf-ruby or C-c C-s from a Ruby buffer
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)
(eval-after-load 'ruby-mode '(add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings))

; Avoid ridiculous Ruby indentation.
(setq ruby-deep-indent-paren nil)

; Show the starting point of a block when I'm at the end.
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

; I don't want autopair working in Ruby buffers ...
(add-hook 'ruby-mode-hook
	  #'(lambda () 
	      (autopair-mode -1))
	  )

; ... because I use electric mode to do that and more.
(require 'ruby-electric)

; Highlight syntax errors on the fly.
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; Rinari (for Ruby on Rails)
; (require 'rinari)

(provide 'setup-ruby)
