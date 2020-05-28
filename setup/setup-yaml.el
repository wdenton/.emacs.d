;; YAML mode
;; https://github.com/yoshiki/yaml-mode

(use-package yaml-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  )

(provide 'setup-yaml)
