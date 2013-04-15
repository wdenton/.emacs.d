;;;
;;; William Denton <wtd@pobox.com>
;;;

;; Look at 
;; https://github.com/magnars/.emacs.d
;; https://gitcafe.com/Leaflet/.emacs.d/blob/master/sane-defaults.el
;; And figure out why auctex completion isn't working

;; Good reading
;; http://tex.stackexchange.com/questions/52179/what-is-your-favorite-emacs-and-or-auctex-command-trick
;; http://tex.stackexchange.com/questions/50827/a-simpletons-guide-to-tex-workflow-with-emacs/50919

;; Managing Packages in Emacs 24
;; http://crn.io/2011/12/managing-packages-in-emacs-24/

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
; (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Turn off the toolbar and scrollbar
; (tool-bar-mode 0)
; (scroll-bar-mode 0)

;; No startup screen
(setq inhibit-startup-message t)

; user-emacs-directory is ~/.emacs.d/
; I keep my local things in ~/.emacs.d/site-lisp/
; and I want everything in there available, including inside
; subdirectories
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(let ((default-directory site-lisp-dir)) (normal-top-level-add-subdirs-to-load-path))

; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;;; Packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

; Make sure that all of the packages I want are installed. If not, install them.
(setq my-packages '(auctex 
		    autopair 
		    color-theme-solarized
		    ess 
		    expand-region 
		    gist
		    markdown-mode 
		    ruby-mode 
		    yaml-mode))
(when (not package-archive-contents) 
  (package-refresh-contents))
(dolist (p my-packages) 
  (when (not (package-installed-p p)) 
    (package-install p)))

;; Configurations

(load-theme 'solarized-dark t)

(setq default-major-mode 'text-mode)
; (setq text-mode-hook 'turn-on-auto-fill)

(setq font-lock-maximum-decoration t)
(setq sentence-end-double-space t)
(set-language-environment "UTF-8")

;; Proper line wrapping
(global-visual-line-mode 1)

;; Highlight current row
(global-hl-line-mode 1)
;; And set its colour
;; (set-face-background hl-line-face "#efefef")

;; Matches parentheses and such in every mode
(show-paren-mode 1)

;; Calendar should start on Monday (not that I ever use the calendar)
(setq calendar-week-start-day 1)


;; I don't want to type in "yes" or "no" - I want y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Refresh buffers when files change (don't worry, changes won't be lost)
(global-auto-revert-mode t)

;; Keep a list of recently opened files
(require 'recentf)
(recentf-mode 1)
(global-set-key "\C-xf" 'recentf-open-files)

;; Split window horizontally, not vertically (I prefer side by side with the newer wider screens)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Enable cutting/pasting and putting results into the X clipboard
(global-set-key [(shift delete)] 'clipboard-kill-region)
(global-set-key [(control insert)] 'clipboard-kill-ring-save)
(global-set-key [(shift insert)] 'clipboard-yank)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

; M-backspace is backward-word-kill, and C-backspace is bound to that
; by default.  Change that to backword-kill-line so it deletes from
; the point to the beginning of the line.
(global-set-key (kbd "C-<backspace>") (lambda ()
  (interactive)
  (kill-line 0)))


;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

; Open a new line above or below the current one, even if the cursor is midsentence.
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))
(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; Automatically load a gist in the browser when I post one.
(defvar gist-view-gist 1)

;; Save file of last session
(desktop-save-mode 1)

;; Keep custom settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;;
;;; Modes.
;;;

; (require 'centered-cursor-mode)

;; Emacs Speaks Statistics (for R)
(require 'ess-site)
(setq ess-use-auto-complete t)
;; Make R go to the width of the Emacs frame
(add-hook 'ess-R-post-run-hook 'ess-execute-screen-options)

(require 'ess-R-object-popup)
(define-key ess-mode-map "\C-c\C-g" 'ess-R-object-popup)

;; nxml for HTML etc.
(add-to-list 'auto-mode-alist
	     (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
		   'nxml-mode))
(fset 'html-mode 'nxml-mode)

;; cperl-mode
(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-level 4)
(setq cperl-continued-statement-offset 3)
(setq cperl-close-paren-offset -2)
(setq cperl-info-on-command-no-prompt t)

;; Ruby mode
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))

;; avoid ridiculous ruby indentation
(setq ruby-deep-indent-paren nil)

;; Show the starting point of a block, when I'm at the end
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

(require 'ruby-electric)

(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; Rinari (for Ruby on Rails)
; (require 'rinari)

;; YAML mode
;; https://github.com/yoshiki/yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'setup-latex)

(require 'setup-autocomplete)

(require 'setup-useful-functions)

;; outline-mode
;; Automatically go into outline mode on any file ending with .outline.txt
(add-to-list 'auto-mode-alist '("\\.outline\\.txt\\'" . outline-mode))
(add-hook 'outline-mode-hook 'hide-body)
(add-hook 'outline-mode-hook 'turn-on-visual-line-mode)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
;; I should know how to add both at the same time
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)

;; expand-region (see https://github.com/magnars/expand-region.el)
;; C-= successively expands the region with great intelligence
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; UTF-8 please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; Various Little Customizations.

;; This is adding in the ":" character to the standard set,
;; which means M-q will work on paragraphs that start with :
;; (my news quote character).
(setq adaptive-fill-regexp "[      ]*\\([#:;>*]+ +\\)?")

;; autopair makes matching parentheses always appear
;; https://code.google.com/p/autopair/
(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers

;; Remove trailing whitespace automatically
; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Highlight marked text - only works under X.
(transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Scroll by one line at a time.
(setq scroll-step 1)

;; Make searches case insensitive.
(setq case-fold-search nil)

;; Nifty parenthesis thing - hit % to toggle between a pair.
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key "%"             'match-paren)           ; % like vi

;; Highlight matching parenthesis whenever the point is over one.
(require 'paren)

;; Make it so down-arrow at the end of a file doesn't add in a new line.
(setq next-line-add-newlines nil)

;; Silently ensure a file ends in a newline when it's saved.
(setq require-final-newline t)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Turn on highlighting for search strings.
(setq search-highlight t)

;; Sentences do not need double spaces to end.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

