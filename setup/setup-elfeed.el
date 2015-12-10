;;;;
;;;; elfeed
;;;;

;; https://github.com/skeeto/elfeed

(load-file "~/.emacs.d/setup/setup-elfeed-subscriptions.el")

(require 'elfeed)

(defun elfeed-search-format-date (date)
  (format-time-string "%d %b %Y" (seconds-to-time date)))

(provide 'setup-elfeed)
