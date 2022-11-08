(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)

(add-to-list 'load-path (expand-file-name "/usr/local/src/org-mode/lisp"))

(org-babel-do-load-languages 'org-babel-load-languages '(
     (R . t)
     (shell . t)
     ))
