;; use-package is needed before init.el can automatically install
;; everything I want.  This is enough to get it in place.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(setq package-archive-priorities '(("melpa" . 20) ("gnu" . 10)))
(package-refresh-contents)
(package-install 'use-package)
