;;;
;;; Polymode, for Markdown + R + Yaml etc.
;;; https://github.com/vspinu/polymode/
;;;

(use-package polymode)

;; Polymode is nice everywhere, except I do not want it in Org.

(use-package poly-markdown
  :config
  (add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))
  (setq markdown-hide-urls t)
  (setq markdown-hide-markup t)
  (setq markdown-url-compose-char "⋯")
  (setq markdown-header-scaling t)
  (add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)
  )

(use-package poly-R
  :config
  (add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r-mode))
  )

(use-package poly-noweb)

(provide 'setup-polymode)
