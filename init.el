;;;;
;;;; William Denton <wtd@pobox.com>
;;;;

;; Look at
;; https://github.com/magnars/.emacs.d
;; https://gitcafe.com/Leaflet/.emacs.d/blob/master/sane-defaults.el
;; https://ogbe.net/emacsconfig.html

;; Org inits:
;; https://github.com/therockmandolinist/dotfiles/blob/master/rc.org
;; https://github.com/thblt/dotfiles/blob/master/.emacs.d/dotemacs.org
;; https://github.com/marcowahl/.emacs.d/blob/master/init.org
;; http://karl-voit.at/2017/06/03/emacs-org/

;; To try
;; pdf-tools: https://github.com/politza/pdf-tools

;;;;
;;;; Turn this on when needed
;;;;
;; (setq debug-on-error t)

;;;;
;;;; Starting up
;;;;

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No startup screen
(setq inhibit-startup-message t)

;; Full screen!
(when (fboundp 'toggle-frame-maximized)
  (toggle-frame-maximized))

;;;;
;;;; Load paths and where to find things
;;;;

;; user-emacs-directory is ~/.emacs.d/
;; I keep my local things in ~/.emacs.d/site-lisp/ and I want
;; everything in there available, including inside subdirectories
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(let ((default-directory site-lisp-dir)) (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/setup"))
;; http://stackoverflow.com/questions/24779041/disable-warning-about-emacs-d-in-load-path
;; (add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; Use Org's current development branch, pulled down with git
;; (http://orgmode.org/org.html#Installation)
;; (add-to-list 'load-path "/usr/local/src/org-mode/lisp")

;; Run the server; now I can load any file into Emacs with
;; 'emacsclient file'
;; Works a treat with the It's All Text! extension in Firefox, too.
(server-mode)

;;;;
;;;; Packages
;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; (package-initialize)

(setq package-archive-priorities
      '(
	("melpa" . 20)
	("gnu" . 10)))

;; Make sure that all of the packages I want are installed. If not, install them.
(setq my-packages '(ag
		    aggressive-indent
		    anzu
		    async
		    auctex
		    color-theme-solarized
		    counsel
		    csv-mode
		    diminish
		    elfeed
		    ess
		    expand-region
		    ;; gh
		    ;; gist
		    highlight
		    ibuffer-projectile
		    ;; ibuffer-vc
		    imenu-anywhere
		    indent-guide
		    ivy
		    js-comint
		    js2-mode
		    json-mode
		    list-unicode-display
		    magit
		    memoize
		    multiple-cursors
		    nov
		    ;; org-reveal ;; install by hand https://github.com/yjwen/org-reveal/
		    osc
		    package-lint
		    pcache
		    powerline
		    ruby-electric
		    sonic-pi
		    swiper
		    undo-tree
		    use-package
		    visual-fill-column
		    wrap-region
		    yaml-mode
		    yari
		    yasnippet
		    yasnippet-snippets
		    ))
(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; use-package
;; https://github.com/jwiegley/use-package
;; Docs say this should go at the start of the init file.
(eval-when-compile
  (require 'use-package))
(require 'diminish) ;; if you use :diminish
;; (require 'bind-key) ;; if you use any :bind variant
;; Make sure that if I want a package, it gets installed automatically.
(setq use-package-always-ensure t)

;;;;
;;;; Package management
;;;;

;; Paradox
;; A nice package management layer.
;; (https://github.com/Malabarba/paradox/)
;; (use-package paradox
;;   :config
;;   ;; I prefer package updates not to happen in the background
;;   (setq paradox-execute-asynchronously nil)
;;   ;; For starring on GitHub (see ~/.bash.$HOSTNAME.rc)
;;   (setq paradox-github-token (getenv "PARADOX_TOKEN"))
;;   ;; But if I install a package, don't automatically star it.
;;   (setq paradox-automatically-star nil)
;;   )

;; Commented because this conflicts with Paradox
;; Don't truncate the names in the Package column when viewing packages
;; (add-hook 'package-menu-mode-hook
;; 	  (lambda()
;; 	    (setq tabulated-list-format
;; 		  [("Package" 28 package-menu--name-predicate)
;; 		   ("Version" 18 nil)
;; 		   ("Status"  10 package-menu--status-predicate)
;; 		   ("Archive" 10 package-menu--archive-predicate)
;; 		   ("Description" 0 nil)])
;; 	    (tabulated-list-init-header)))

;;;;
;;;; System tweaks
;;;;

;; Don't garbage clean so often
(setq gc-cons-threshold 100000000)

;;;;
;;;; Parentheses!
;;;;

;; smartparens
;; https://github.com/Fuco1/smartparens/
(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  `(sp-show-pair-match-face :inverse-video t :bold t)
  `(sp-show-pair-mismatch-face :inverse-video t :bold t)
  )

;;;;
;;;; Indenting
;;;;
(electric-indent-mode 1)

;; aggressive-indent mode is aggressive indeed, but very handy.
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

;;;;
;;;; Faces 'n' fonts
;;;;

;; Measured in 0.1 pts
(setq wtd-font-height-for-this-machine 130)
(when (string= system-name "work")
  ;; Need it a little bigger on my work machine
  (setq wtd-font-height-for-this-machine 160))

;; (set-face-attribute 'default nil :height wtd-font-height-for-this-machine)
(set-face-attribute 'default nil :font "Ubuntu Mono" :height wtd-font-height-for-this-machine)

;;;;
;;;; Theme: solarized
;;;;

;; Light on dark theme; soothing to my eyes
;; https://github.com/sellout/emacs-color-theme-solarized
(load-theme 'solarized t)
(set-frame-parameter nil 'background-mode 'dark)
(enable-theme 'solarized)

;; Keep custom settings in separate file
;; If custom.el doesn't exist, create an empty file
;; (for starting from scratch).
(setq custom-file "~/.custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; Always with the font-locking
(setq font-lock-maximum-decoration t)

;; Sentences do not need double spaces to end.  (But it's better when they do.)
(set-default 'sentence-end-double-space nil)

;; UTF-8 please (surely this is overkill?)
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Proper line wrapping
(global-visual-line-mode 1)

;; Highlight current row
(global-hl-line-mode 1)
;; And set its colour
;; (set-face-background hl-line-face "#efefef")

;; Calendar should start on Monday
(setq calendar-week-start-day 1)

;; I don't want to type in "yes" or "no"---I want y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Lower the time to echo keystrokes
(setq echo-keystrokes 0.1)

;; Never use an X dialog box; use the minibuffer instead
(setq use-dialog-box nil)

;; S-<return> to insert line, indent it, and move the cursor there.
;; http://emacsredux.com/blog/2013/03/26/smarter-open-line/
(defun wtd/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key (kbd "s-<return>") 'wtd/smart-open-line)

;; Break out of default (especially Org) indentation, and just start a new line.
(global-set-key (kbd "C-M-<return>") 'electric-indent-just-newline)

;; Split window horizontally, not vertically (I prefer side by side
;; with the newer wider screens)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Make window splitting more useful
;; http://pages.sachachua.com/.emacs.d/Sacha.html
(defun wtd/vsplit-last-buffer (PREFIX)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (unless prefix
    (switch-to-next-buffer)))
(defun wtd/hsplit-last-buffer (PREFIX)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (unless prefix (switch-to-next-buffer)))
(global-set-key (kbd "C-x 2") 'wtd/vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'wtd/hsplit-last-buffer)

;; Enable cutting/pasting and putting results into the X clipboard
(global-set-key [(shift delete)] 'clipboard-kill-region)
(global-set-key [(control insert)] 'clipboard-kill-ring-save)
(global-set-key [(shift insert)] 'clipboard-yank)

;; Allow pasting selection outside of Emacs
(setq select-enable-clipboard t)

;; M-backspace is backward-word-kill, and C-backspace is bound to that
;; by default. Change that to backword-kill-line so it deletes from
;; the point to the beginning of the line.
(global-set-key (kbd "C-<backspace>") (lambda ()
					(interactive)
					(kill-line 0)))

;;;;
;;;; Save where I was and what I had open
;;;;

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Remember all the buffers I have open
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

;;;; nxml for HTML etc.
(add-to-list 'auto-mode-alist
	     (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
		   'nxml-mode))
(fset 'html-mode 'nxml-mode)

;;;;
;;;; *scratch* buffer
;;;;

;; The *scratch* buffer is lisp-interaction-mode by default, but I use Org more.
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "")

;; C-c b to create a new scratch buffer.  Default to org-mode.
;; From milkypostman.
;; See also https://emacs.stackexchange.com/a/81/145 and maybe try to glom the two together
(defun create-scratch-buffer nil
  "Create a new scratch buffer to work in (could be *scratch* - *scratchX*)."
  (interactive)
  (let ((n 0)
	bufname)
    (while (progn
	     (setq bufname (concat "*scratch"
				   (if (= n 0) "" (int-to-string n))
				   "*"))
	     (setq n (1+ n))
	     (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (org-mode)))
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;;;
;;; One-line customizations
;;;

;; expand-region (see https://github.com/magnars/expand-region.el)
;; C-= successively expands the region with great intelligence
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; "The command other-window is normally bound to C-x o but I find
;; that way too cumbersome for what is such a frequent operation. M-o
;; is normally bound to some rich text formatting nobody cares about."
;; http://www.masteringemacs.org/articles/2014/02/28/my-emacs-keybindings/
(global-set-key (kbd "M-o") 'other-window)

;; Highlight marked text - only works under X.
(transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; I sometimes use CamelCase in Ruby and R
(add-hook 'prog-mode-hook 'subword-mode)

;; Scroll by one line at a time.
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Scrolling-only-one-line.html
(setq scroll-conservatively 10000)

;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag
(setq auto-window-vscroll nil)

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
;; http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; When I want to kill a buffer, it's always the current one, so don't ask.
;; http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;;;
;;;; Narrowing to region
;;;;

;; I'm old enough to be able to use narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; This is taken from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if you
         ;; don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(define-key ctl-x-map "w" #'narrow-or-widen-dwim)
(eval-after-load 'latex '(define-key LaTeX-mode-map "\C-xw" nil))

;; rainbow-mode
;; "All strings representing colors will be highlighted with the color they represent."
(use-package rainbow-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode)
  :config
  (rainbow-mode t) ;; #0af
  )

;; Include the size of the file in the mode line
(size-indication-mode t)

;; Show which column I'm in in the mode line as well
(column-number-mode t)

;; 8 is wrong
(setq tab-width 4)

;; Make the cursor a thin horizontal bar, not a block (but I still
;; like it blinking)
;; Can be 'bar or 'box or '(hbar . 3) etc.
(set-default 'cursor-type '(bar . 4))

;; Use C-c left or C-c right to go back and forth in window
;; configurations
(winner-mode t)

;; anzu-mode provides a "minor mode which display current point and
;; total matched in various search mode."
;; https://github.com/syohex/emacs-anzu
(global-anzu-mode t)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; I was getting errors about exceeding the defaults on both of these.
(setq max-specpdl-size 50000)
(setq max-lisp-eval-depth 25000)

;; Settings for command interpreter modes, which I use mostly for R
;; and Ruby.
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
;;(setq comint-prompt-read-only t)

;; undo-tree-mode
(global-undo-tree-mode)

;; Let me upcase or downcase a region, which is disabled by default.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; prettify-symbols-mode was introduced in 24.4
(global-prettify-symbols-mode +1)
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Stop magit from nagging me about a change
(setq magit-last-seen-setup-instructions "1.4.0")

;; imenu
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-.") 'imenu-anywhere)

;; Disable byte-compile warnings, which I don't care about.
;; http://tsengf.blogspot.ca/2011/06/disable-byte-compile-warning-in-emacs.html
(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  redefine
                                  obsolete
                                  noruntime
                                  cl-functions
                                  interactive-only
                                  ))

;; wrap-region to wrap regions in * or / etc.
;; See http://pragmaticemacs.com/emacs/wrap-text-in-custom-characters/
(wrap-region-mode t)
(wrap-region-add-wrappers
 '(("*" "*" nil org-mode)
   ("~" "~" nil org-mode)
   ("/" "/" nil org-mode)
   ("=" "=" "+" org-mode)
   ("_" "_" nil org-mode)
   ("$" "$" nil (org-mode latex-mode))))

;;;;
;;;; The mode-line
;;;;

;;;; Tidy up the mode-line.  I don't need to see everything in there.
;; (require 'diminish) ;; At top of file, where use-pockage is called.
(eval-after-load "abbrev"            '(diminish 'abbrev-mode))
(eval-after-load "aggressive-indent" '(diminish 'aggressive-indent-mode)) ;; →
(eval-after-load "anzu"              '(diminish 'anzu-mode))
(eval-after-load "auto-complete"     '(diminish 'auto-complete-mode)) ;; α
(eval-after-load "autorevert"        '(diminish 'auto-revert-mode))
(eval-after-load "eldoc"             '(diminish 'eldoc-mode))
;; (eval-after-load "flymake"           '(diminish 'flymake-mode))
(eval-after-load "indent-guide"      '(diminish 'indent-guide-mode))
;;(eval-after-load "magit"             '(diminish 'magit-auto-revert-mode))
(eval-after-load "org-indent"        '(diminish 'org-indent-mode)) ;; →
(eval-after-load "rainbow-mode"      '(diminish 'rainbow-mode))
(eval-after-load "rubocop"           '(diminish 'rubocop-mode))
(eval-after-load "ruby-block"        '(diminish 'ruby-block-mode))
(eval-after-load "simple"            '(diminish 'visual-line-mode))
(eval-after-load "smerge-mode"       '(diminish 'smerge-mode))
(eval-after-load "subword"           '(diminish 'subword-mode))
(eval-after-load "undo-tree"         '(diminish 'undo-tree-mode))
(eval-after-load "wrap-region"       '(diminish 'wrap-region-mode))

;;;; powerline: Fancy up the modeline!
;; https://github.com/jonathanchu/emacs-powerline
;; Sticking with the default seems to be nice enough for me.

(require 'powerline)
(powerline-default-theme)

;;;;
;;;; amx for completion
;;;;
(use-package amx
  :ensure t
  :custom
  (amx-backend 'ivy)
  )

;;;;
;;;; Ivy and Swiper
;;;;

;; https://github.com/abo-abo/swiper
;; See also http://irreal.org/blog/?p=6419

(use-package swiper
  :ensure t
  :diminish ivy-mode
  :diminish ivy
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-M-i" . complete-symbol)
         ("C-." . counsel-imenu)
         ("C-c 8" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c k" . counsel-ag)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("M-y" . counsel-yank-pop))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d "))

;;;;
;;;; Projectile (https://github.com/bbatsov/projectile)
;;;;

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-mode-line '(:eval (format " ᴨ(%s)" (projectile-project-name))))
  )

;;;;
;;;; launcher-map, to make it easier to run common things
;;;;

;; http://endlessparentheses.com/launcher-keymap-for-standalone-features.html
(define-prefix-command 'launcher-map)
(define-key ctl-x-map "l" 'launcher-map)
(define-key launcher-map "c" #'calculator) ; calc is too much
;;(define-key launcher-map "d" #'ediff-buffers)
;;(define-key launcher-map "f" #'find-dired)
(define-key launcher-map "g" #'magit-status)
(define-key launcher-map "l" #'goto-line)
(define-key launcher-map "m" #'mc/edit-lines)
;; (define-key launcher-map "p" #'paradox-list-packages) ;; Or just 'list-packages if not using Paradox
(define-key launcher-map "p" #'list-packages) ;; Or just 'list-packages if not using Paradox
(define-key launcher-map "s" #'eshell)
;;(define-key launcher-map "t" #'proced) ; top
(define-key launcher-map "u" #'magit-pull-from-upstream)
(define-key launcher-map "w" #'count-words-region)

;; Perhaps try launching other programs?
;; http://endlessparentheses.com/keymap-for-launching-external-applications-and-websites.html

;;;;
;;;; EPUB
;;;;

(push '("\\.epub\\'" . nov-mode) auto-mode-alist)

;;;;
;;;; YASnippet
;;;;

;; https://joaotavora.github.io/yasnippet/

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config

  (use-package yasnippet-snippets
    :ensure t)

  (yas-global-mode 1)
  )

;;;;
;;;; Git stuff
;;;;

;; git-gutter
;; https://github.com/syohex/emacs-git-gutter
;; (use-package git-gutter
;;   :ensure t
;;   :diminish git-gutter-mode
;;   :config
;;   (global-git-gutter-mode t)
;;   )

;; git-gutter
;; https://github.com/syohex/emacs-git-gutter
(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t)
  (setq git-gutter-fr:side 'right-fringe)
  )

;;;;
;;;; which-key
;;;;

;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  )

;;;;
;;;; flycheck
;;;;
;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode)
;;   :diminish flycheck-mode
;;   :config
;;   (setq flycheck-global-modes '(not org-mode))
;;   ;; Could also set :modes to list where I want it.
;;   )

;;;;
;;;; Copying and pasting with volatile-mode
;;;;
(use-package volatile-highlights
  :ensure t
  :init (volatile-highlights-mode t)
  :diminish volatile-highlights-mode
  :config
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  )

;;;;
;;;; esonify
;;;;

;; "An Emacs extension that sonifies your code."
;; https://github.com/oflatt/esonify
;; M-x esonify-mode to toggle on/off
(use-package esonify
  :ensure t
  )

;;;;
;;;; Window management
;;;;

;; eyebrowse
;; https://github.com/wasamasa/eyebrowse
(use-package eyebrowse
  :ensure t
  ;; :diminish eyebrowse-mode
  :init
  (eyebrowse-setup-opinionated-keys)
  :config
  (eyebrowse-mode t)
  ;; (setq eyebrowse-new-workspace t)
  (setq eyebrowse-wrap-around t)
  )

;;;;
;;;; Mode-specific customizations
;;;;

(require 'setup-completion)
(require 'setup-elfeed)
(require 'setup-eshell)
(require 'setup-ess)
(require 'setup-expansions)
(require 'setup-file-management)
(require 'setup-ibuffer)
;; (require 'setup-javascript)
(require 'setup-jekyll)
(require 'setup-latex)
(require 'setup-markdown)
(require 'setup-orgmode)
(require 'setup-ruby)
(require 'setup-shell-scripts)
(require 'setup-sonic-pi)
(require 'setup-useful-functions)
(require 'setup-yaml)
;; (require 'setup-polymode) ;; Keep this last, after Markdown stuff is defined.
