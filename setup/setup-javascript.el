;; Configuration for Org mode (http://orgmode.org/)

;; js2-mode is a better Javascript mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; js-comint lets you run a Javascript REPL inside Emacs (http://js-comint-el.sourceforge.net/)
(require 'js-comint)

(setq inferior-js-program-command "nodejs")
; (setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")
(add-hook 'js2-mode-hook '(lambda ()
							(local-set-key "\C-x\C-e" 'js-send-last-sexp)
							(local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
							(local-set-key "\C-cb" 'js-send-buffer)
							(local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
							(local-set-key "\C-cl" 'js-load-file-and-go)
							))
(setenv "NODE_NO_READLINE" "1")

;; (setq inferior-js-mode-hook
;;       (lambda ()
;;         ;; We like nice colors
;;         (ansi-color-for-comint-mode-on)
;;         ;; Deal with some prompt nonsense
;;         (add-to-list
;;          'comint-preoutput-filter-functions
;;          (lambda (output)
;;            (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))))



(provide 'setup-javascript)
