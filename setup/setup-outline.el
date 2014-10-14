;; outline-mode
;; Automatically go into outline mode on any file ending with .outline.txt
(add-to-list 'auto-mode-alist '("\\.outline\\.txt\\'" . outline-mode))
(add-hook 'outline-mode-hook 'hide-body)
(add-hook 'outline-mode-hook 'turn-on-visual-line-mode)

(provide 'setup-outline)
