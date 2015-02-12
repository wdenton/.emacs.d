;;;; Customizations for doing things with files, in dired and related modes

;; Keep a list of recently opened files
;; C-x f show the list, and it's easy to pick from it (or q to quit it)
(require 'recentf)
(recentf-mode 1)
(global-set-key "\C-xf" 'recentf-open-files)

;; Refresh buffers when files change (don't worry, changes won't be lost)
(global-auto-revert-mode t)

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
					; (setq auto-revert-verbose nil)

;; With this, C-x C-j (M-x dired-jump) goes to the current file's position in a dired buffer
;; (http://emacsredux.com/blog/2013/09/24/dired-jump/)
(require 'dired-x)

;; Emacs 24.4 defaults to an ls -1 view, not ls -l, but I want details.
;; This needs to be specified before requiring dired+ (see http://irreal.org/blog/?p=3341)
(setq diredp-hide-details-initially-flag nil)

;; More extra stuff
(require 'dired+)

;; dired+ has got some crazy colours by default. This turns that off, but leaves the settings at maximum (the default)
;; for everything else
(setq font-lock-maximum-decoration (quote ((dired-mode) (t . t))))

;; "Don't show uninteresting files in Emacs completion window"
;; When the buffer of possible files open, it shows all matches.
;; If I'm looking for foo.ext and run 'C-x C-f fo TAB' it will show foo.ext and foo.ext~.
;; Because ~ is in completion-ignored-extensions it won't try to open foo.ext~, but I'd rather
;; not see it in the first place.
;;
;; This solution comes from http://stackoverflow.com/questions/1731634/dont-show-uninteresting-files-in-emacs-completion-window
(defadvice completion--file-name-table (after
					ignoring-backups-f-n-completion
                                        activate)
  "Filter out results when they match `completion-ignored-extensions'."
  (let ((res ad-return-value))
    (if (and (listp res)
	     (stringp (car res))
	     (cdr res))                 ; length > 1, don't ignore sole match
	(setq ad-return-value
              (completion-pcm--filename-try-filter res)))))

;; File management shortcuts (from Bodil Stokke's setup: https://github.com/bodil/emacs.d)
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(provide 'setup-file-management)
