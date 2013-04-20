;; Ruby mode
(autoload 'ruby-mode "ruby-mode" "Mode for editing Ruby")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

; (autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")

(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)
(eval-after-load 'ruby-mode '(add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings))

;; avoid ridiculous Ruby indentation
(setq ruby-deep-indent-paren nil)

;; Show the starting point of a block, when I'm at the end
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

; (require 'ruby-electric)

(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; Rinari (for Ruby on Rails)
; (require 'rinari)

(provide 'setup-ruby)
