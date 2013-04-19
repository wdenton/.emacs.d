;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
;; I should know how to add both at the same time
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)

(provide 'setup-markdown)
