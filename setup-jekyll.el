;; Configuration for Jekyll (http://jekyllrb.com/)

(defun jekyll-timestamp ()
  "Insert a time stamp suitable for use in a Jekyll page or post.  Replaces current text selection."
  (interactive)
  (when (region-active-p) (delete-region (region-beginning) (region-end) ) )
  (insert (format-time-string "%Y-%m-%d %H:%M:%S %z")))

(provide 'setup-jekyll)
