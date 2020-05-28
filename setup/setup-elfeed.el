;;;;
;;;; elfeed
;;;;

;; https://github.com/skeeto/elfeed

(use-package elfeed
  :config
  ;; See example file for what this should look like
  (setq elfeed-subscription-list "~/.emacs.d/setup/setup-elfeed-subscriptions.el")
  (if (file-exists-p elfeed-subscription-list)
      (load-file elfeed-subscription-list))
  (defun elfeed-search-format-date (date)
    ;; Show date as "10 Dec 2015"
    (format-time-string "%d %b %Y" (seconds-to-time date)))
  ;; Stop feed fetches from timing out so quickly
  (setf url-queue-timeout 30)
  ;; Turn on curl, which is new and not the default
  (setq elfeed-use-curl t)
  )

(provide 'setup-elfeed)
