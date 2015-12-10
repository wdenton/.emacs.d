;;;;
;;;; elfeed
;;;;

;; https://github.com/skeeto/elfeed

(require 'elfeed)

;; See example file for what this should look like
(load-file "~/.emacs.d/setup/setup-elfeed-subscriptions.el")

;; Show date as "10 Dec 2015"
(defun elfeed-search-format-date (date)
  (format-time-string "%d %b %Y" (seconds-to-time date)))

;; Stop feed fetches from timing out so quickly
(setf url-queue-timeout 30)

(provide 'setup-elfeed)
