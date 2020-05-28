;; YAML mode
;; https://github.com/yoshiki/yaml-mode

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  )

(provide 'setup-yaml)
