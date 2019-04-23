;;;;
;;;; elfeed
;;;;

;; https://github.com/skeeto/elfeed

(require 'elfeed)

;; See example file for what this should look like
(setq elfeed-subscription-list "~/.emacs.d/setup/setup-elfeed-subscriptions.el")
(if (file-exists-p elfeed-subscription-list)
    (load-file elfeed-subscription-list))

;; Show date as "10 Dec 2015"
(defun elfeed-search-format-date (date)
  (format-time-string "%d %b %Y" (seconds-to-time date)))

;; Stop feed fetches from timing out so quickly
(setf url-queue-timeout 30)

;; Turn on curl, which is new and not the default
(setq elfeed-use-curl t)

(provide 'setup-elfeed)
