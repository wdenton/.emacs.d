;;;;
;;;; Configuration for Markdown
;;;;

;; https://github.com/jrblevin/markdown-mode

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-hide-urls t)
  (setq markdown-hide-markup t)
  (setq markdown-url-compose-char "⋯")
  (add-hook 'markdown-mode-hook 'turn-on-outline-minor-mode)
  (add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)
  )

(provide 'setup-markdown)
