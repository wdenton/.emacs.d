;;;;
;;;; Expansions and abbreviations
;;;;

;; abbrev

(setq-default abbrev-mode t)

;; Where to store the abbrevs.
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

;; Save abbrevs when files are saved
(setq save-abbrevs t)

;; Don't ask whether to save abbrevs when quitting Emacs
(setq save-abbrevs nil)

(provide 'setup-expansions)
