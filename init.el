;;;
;;; William Denton <wtd@pobox.com>
;;;

;; Look at
;; https://github.com/magnars/.emacs.d
;; https://gitcafe.com/Leaflet/.emacs.d/blob/master/sane-defaults.el
;; And figure out why auctex completion isn't working

;; Only on GitHub, possibly worth getting:
;; google-maps.el http://julien.danjou.info/projects/emacs-packages#google-maps

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No startup screen
(setq inhibit-startup-message t)

; user-emacs-directory is ~/.emacs.d/
; I keep my local things in ~/.emacs.d/site-lisp/
; and I want everything in there available, including inside subdirectories
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(let ((default-directory site-lisp-dir)) (normal-top-level-add-subdirs-to-load-path))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; This is bound to f11 in Emacs 24.4
; (toggle-frame-fullscreen)

;; Run the server; now I can load any file into Emacs with 'emacsclient file'
;; Works a treat with the It's All Text! extension in Firefox.
(server-mode)

;;;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Make sure that all of the packages I want are installed. If not, install them.
(setq my-packages '(anzu
		    auctex
		    auto-complete
		    auto-complete-auctex
		    ;autopair
		    color-theme-solarized
		    dired+
		    ess
		    ess-R-object-popup
		    expand-region
		    flymake-ruby
		    gh
		    gist
		    ibuffer-vc
		    inf-ruby
		    js-comint
		    js2-mode
		    magit
		    markdown-mode
		    multiple-cursors
		    org
		    org-bullets
		    ; org-reveal ;; install by hand https://github.com/yjwen/org-reveal/
		    pcache
		    rainbow-mode
		    rubocop
		    ruby-block
		    ruby-electric
		    ruby-mode
		    rvm
		    smartparens
		    undo-tree
		    yaml-mode
		    yasnippet
		    zotelo
		    ))
(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;; Parentheses-handling stuff

;; Highlight matching parenthesis whenever the point is over one.
;;;;; (require 'paren)

;; Matches parentheses and such in every mode
;;;;; (show-paren-mode 1)
;;;;; (setq show-paren-style 'mixed) ; Values; 'expression, 'parenthesis or 'mixed

;; Make matching parentheses and quotes always appear
;; https://github.com/capitaomorte/autopair
;(require 'autopair)
;(autopair-global-mode) ;; to enable in all buffers

;; smartparens for good handling of parentheses (https://github.com/Fuco1/smartparens/)
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode +1)

;; Nifty parenthesis thing - hit % to toggle between a pair.
;(defun match-paren (arg)
;  "Go to the matching parenthesis if on parenthesis, otherwise insert %."
;  (interactive "p")
;  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;        (t (self-insert-command (or arg 1)))))
;(global-set-key "%"             'match-paren)           ; % like vi

;; I don't like the way this works with Org.
;; TODO Fix this problem.
(electric-indent-mode 1)
;; From: http://foldl.me/2012/disabling-electric-indent-mode/
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'electric-indent-functions)
;;                  (list (lambda (arg) 'no-indent)))))

;; Light on dark theme; soothing to my eyes
(load-theme 'solarized-dark t)

; (setq default-major-mode 'text-mode)

(setq font-lock-maximum-decoration t)

;; Sentences do not need double spaces to end.
(set-default 'sentence-end-double-space nil)

;; UTF-8 please (surely this is overkill?)
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Some modes should use variable pitch fonts.
;; (dolist (hook '(erc-mode-hook
;; 		LaTeX-mode-hook
;; 		org-mode-hook
;; 		markdown-mode-hook))
;;   (add-hook hook (lambda () (variable-pitch-mode t))))

;; Proper line wrapping
(global-visual-line-mode 1)

;; Highlight current row
(global-hl-line-mode 1)
;; And set its colour
;; (set-face-background hl-line-face "#efefef")

;; Calendar should start on Monday (not that I ever use the calendar)
(setq calendar-week-start-day 1)

;; I don't want to type in "yes" or "no" - I want y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Split window horizontally, not vertically (I prefer side by side with the newer wider screens)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Enable cutting/pasting and putting results into the X clipboard
(global-set-key [(shift delete)] 'clipboard-kill-region)
(global-set-key [(control insert)] 'clipboard-kill-ring-save)
(global-set-key [(shift insert)] 'clipboard-yank)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; M-backspace is backward-word-kill, and C-backspace is bound to that
;; by default.  Change that to backword-kill-line so it deletes from
;; the point to the beginning of the line.
(global-set-key (kbd "C-<backspace>") (lambda ()
  (interactive)
  (kill-line 0)))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Save all of the buffers I'm working on, for next time
(desktop-save-mode 1)
(setq history-length 50)
(setq desktop-buffers-not-to-save
      (concat "\\("
	      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
	      "\\|\\.emacs.*\\|\\.diary\\|elpa\/*\\|\\.bbdb"
	      "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;; Keep custom settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; nxml for HTML etc.
(add-to-list 'auto-mode-alist
       (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
       'nxml-mode))
(fset 'html-mode 'nxml-mode)

(require 'setup-autocomplete)
(require 'setup-eshell)
(require 'setup-ess)
(require 'setup-file-management)
;(require 'setup-fringe)
(require 'setup-ibuffer)
(require 'setup-javascript)
(require 'setup-jekyll)
(require 'setup-latex)
(require 'setup-markdown)
(require 'setup-orgmode)
(require 'setup-outline)
(require 'setup-ruby)
(require 'setup-useful-functions)
(require 'setup-yaml)

;; The *scratch* buffer is lisp-interaction-mode by default, but I use Org more.
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "")

;; expand-region (see https://github.com/magnars/expand-region.el)
;; C-= successively expands the region with great intelligence
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; "The command other-window is normally bound to C-x o but I find
;; that way too cumbersome for what is such a frequent operation. M-o
;; is normally bound to some rich text formatting nobody cares about."
;; --- http://www.masteringemacs.org/articles/2014/02/28/my-emacs-keybindings/
(global-set-key (kbd "M-o") 'other-window)

;; Make the little calculator easier to get to. I use it quite a bit.
;; Remember you can hit 'q' to quit the calculator.
(global-set-key (kbd "<f1>") 'calculator)

;; Highlight marked text - only works under X.
(transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Scroll by one line at a time.
(setq scroll-step 1)

;; Make searches case insensitive.
(setq case-fold-search nil)

;; Turn on highlighting for search strings.
(setq search-highlight t)

;; Remove trailing whitespace automatically
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Down-arrow at the end of a file doesn't add in a new line.
(setq next-line-add-newlines nil)

;; Silently ensure a file ends in a newline when it's saved.
(setq require-final-newline t)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Make script files executable automatically
; http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; I'm old enough to be able to use narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; "All strings representing colors will be highlighted with the color they represent."
(rainbow-mode t)

;; Include the size of the file in the mode line
(size-indication-mode t)

;; Show which column I'm in in the mode line as well
(column-number-mode t)

;; 8 is wrong
(setq tab-width 4)

;; Make the cursor a thin bar, not a block (but I still like it blinking)
(setq cursor-type 'bar)

;; Use C-c left or C-c right to go back and forth in window configurations
(winner-mode t)

;; anzu-mode provides a "minor mode which display current point and total matched in various search mode."
;; https://github.com/syohex/emacs-anzu
(global-anzu-mode t)

;; Automatically load a gist in the browser when I post one.
(defvar gist-view-gist 1)

;; M-x magit-status is tedious
(global-set-key (kbd "C-x g") 'magit-status)

;; I was getting errors about exceeding the defaults on both of these.
(setq max-specpdl-size 50000)
(setq max-lisp-eval-depth 25000)

;; Settings for command interpreter modes, which I use mostly for R and Ruby.
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
;(setq comint-prompt-read-only t)

;; In case I experiment with Gnus
(setq gnus-home-directory "~/.emacs.d/gnus.d/")
(setq gnus-directory "~/.emacs.d/gnus.d/News/")
(setq message-directory "~/.emacs.d/gnus.d/Mail/")
(setq nnfolder-directory "~/.emacs.d/gnus.d/Mail/archive/")
(setq gnus-init-file "~/.emacs.d/gnus.d/init.el")

;;; ido

;; I'm trying out ido-mode
;; See http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Don't ask for confirmation if I create a new buffer with C-x b)
(setq ido-create-new-buffer 'always)

;; Customize the order of files shown in the minibuffer
(setq ido-file-extensions-order '(".org" ".rb" ".ini"))

;; Make ido use completion-ignored-extensions
(setq ido-ignore-extensions t)

; (require 'twittering-mode)
