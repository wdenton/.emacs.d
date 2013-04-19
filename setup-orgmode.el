; Nice bullets!
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

; Hide the /italics/ and *bold* markers
(setq org-hide-emphasis-markers t)

(provide 'setup-orgmode)
