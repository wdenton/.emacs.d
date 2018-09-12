;;;;
;;;; Configuration for Markdown
;;;;

;; https://github.com/jrblevin/markdown-mode

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ;; Defined elsewhere: use polymode
         ;; ("\\.md\\'" . markdown-mode)
         ;; ("\\.markdown\\'" . markdown-mode))
  :init
  ;; (setq markdown-command "multimarkdown")
  (setq markdown-hide-urls t)
  (setq markdown-hide-markup t)
  (setq markdown-url-compose-char "⋯")
  (setq markdown-header-scaling t)
  ;; (add-hook 'markdown-mode-hook 'turn-on-outline-minor-mode)
  (add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)
  ;; (add-hook 'markdown-mode-hook (lambda () (setq display-line-numbers 'relative)))
  ;; (add-hook 'markdown-mode-hook (lambda () (variable-pitch-mode t))
  )

(provide 'setup-markdown)
