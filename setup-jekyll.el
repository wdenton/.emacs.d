;; Configuration for Jekyll (http://jekyllrb.com/)

(defun jekyll-timestamp ()
  "Update existing date: timestamp on a Jekyll page or post."
  (interactive)
  (save-excursion (
		   goto-char 1)
		  (re-search-forward "^date:")
		  (let ((beg (point)))
		    (end-of-line)
		    (delete-region beg (point)))
		  (insert (concat " " (format-time-string "%Y-%m-%d %H:%M:%S %z"))))
  )
;; TODO: Make the function add a date variable if none exists.

;; (defun jekyll-timestamp ()
;;   "Insert a time stamp suitable for use in a Jekyll page or post.  Replaces current text selection."
;;   (interactive)
;;   (when (region-active-p) (delete-region (region-beginning) (region-end) ) )
;;   (insert (format-time-string "%Y-%m-%d %H:%M:%S %z")))

;; All of the below is taken from http://www.gorgnegre.com/linux/using-emacs-orgmode-to-blog-with-jekyll.html
;; (Later tweaked a bit.)

(global-set-key (kbd "C-c j n") 'jekyll-draft-post)
(global-set-key (kbd "C-c j p") 'jekyll-publish-post)
(global-set-key (kbd "C-c j t") 'jekyll-timestamp)
(global-set-key (kbd "C-c j o") (lambda () (interactive) (find-file "~/web/")))

; (global-set-key (kbd "C-c j p") (lambda () (interactive) (find-file "~/web/_posts/")))
; (global-set-key (kbd "C-c j d") (lambda () (interactive) (find-file "~/web/_drafts/")))

(defvar jekyll-directory "~/web/" "Path to Jekyll blog.")
(defvar jekyll-drafts-dir "_drafts/" "Relative path to drafts directory.")
(defvar jekyll-posts-dir "_posts/" "Relative path to posts directory.")
(defvar jekyll-post-ext ".md"  "File extension of Jekyll posts.")
(defvar jekyll-post-template "---\nlayout: post\ntitle: %s\ntags:\ndate: \n---\n"
  "Default template for Jekyll posts. %s will be replace by the post title.")

(defun jekyll-make-slug (s) "Turn a string into a slug."
  (replace-regexp-in-string " " "-"  (downcase (replace-regexp-in-string "[^A-Za-z0-9 ]" "" s))))

(defun jekyll-yaml-escape (s) "Escape a string for YAML."
  (if (or (string-match ":" s) (string-match "\"" s)) (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"") s))

(defun jekyll-draft-post (title) "Create a new Jekyll blog post."
  (interactive "sPost Title: ")
  (let ((draft-file (concat jekyll-directory jekyll-drafts-dir
                            (jekyll-make-slug title)
                            jekyll-post-ext)))
    (if (file-exists-p draft-file)
        (find-file draft-file)
      (find-file draft-file)
      (insert (format jekyll-post-template (jekyll-yaml-escape title))))))

(defun jekyll-publish-post () "Move a draft post to the posts directory, and rename it so that it contains the date."
  (interactive)
  (cond
   ((not (equal
          (file-name-directory (buffer-file-name (current-buffer)))
          (expand-file-name (concat jekyll-directory jekyll-drafts-dir))))
    (message "This is not a draft post.")
    (insert (file-name-directory (buffer-file-name (current-buffer))) "\n"
            (concat jekyll-directory jekyll-drafts-dir)))
   ((buffer-modified-p)
    (message "Can't publish post; buffer has modifications."))
   (t
    (let ((filename
           (concat jekyll-directory jekyll-posts-dir
                   (format-time-string "%Y-%m-%d-")
                   (file-name-nondirectory
                    (buffer-file-name (current-buffer)))))
          (old-point (point)))
      (rename-file (buffer-file-name (current-buffer))
                   filename)
      (kill-buffer nil)
      (find-file filename)
      (set-window-point (selected-window) old-point)))))

(provide 'setup-jekyll)
