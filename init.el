;;;;
;;;; William Denton <wtd@pobox.com>
;;;;

;; Look at
;; https://github.com/magnars/.emacs.d
;; https://gitcafe.com/Leaflet/.emacs.d/blob/master/sane-defaults.el
;; https://ogbe.net/emacsconfig.html
;; https://github.com/therockmandolinist/dotfiles/blob/master/rc.org
;; https://github.com/thblt/dotfiles/blob/master/.emacs.d/dotemacs.org

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
(add-to-list 'load-path "/usr/local/src/org-mode/lisp")

;; Run the server; now I can load any file into Emacs with
;; 'emacsclient file'
;; Works a treat with the It's All Text! extension in Firefox, too.
(server-mode)

;;;;
;;;; Packages
;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq package-archive-priorities
      '(
	("melpa" . 20)
        ("gnu" . 10)))

;; Make sure that all of the packages I want are installed. If not, install them.
(setq my-packages '(ac-inf-ruby
		    ag
		    aggressive-indent
		    anzu
		    async
		    auctex
		    auto-complete
		    auto-complete-auctex
		    color-theme-solarized
		    counsel
		    csv-mode
		    diminish
		    dired+
		    elfeed
		    ess
		    ess-R-object-popup
		    expand-region
		    flx-ido
		    flymake-ruby
		    ;; gh
		    ;; gist
		    highlight
		    ibuffer-projectile
		    ;; ibuffer-vc
		    ido-ubiquitous
		    ido-vertical-mode
		    imenu-anywhere
		    indent-guide
		    inf-ruby
		    ivy
		    js-comint
		    js2-mode
		    json-mode
		    list-unicode-display
		    magit
		    markdown-mode
		    multiple-cursors
		    ;; org
		    org-bullets
		    ;; org-reveal ;; install by hand https://github.com/yjwen/org-reveal/
		    osc
		    pcache
		    powerline
		    ;; rainbow-identifiers ;; Too much
		    projectile
		    rainbow-delimiters
		    rbenv
		    rubocop
		    ruby-block
		    ruby-electric
		    ruby-mode
		    ;; rvm
		    smex
		    sonic-pi
		    swiper
		    undo-tree
		    use-package
		    visual-fill-column
		    wrap-region
		    yaml-mode
		    yasnippet
		    zotelo
		    zotxt
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
(use-package paradox
  :config
  ;; I prefer package updates not to happen in the background
  (setq paradox-execute-asynchronously nil)
  ;; For starring on GitHub (see ~/.bash.$HOSTNAME.rc)
  (setq paradox-github-token (getenv "PARADOX_TOKEN"))
  ;; But if I install a package, don't automatically star it.
  (setq paradox-automatically-star nil)
  )

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
;;;; Parentheses!
;;;;

;; Highlight matching parenthesis whenever the point is over one.
;; (require 'paren)

;; Matches parentheses and such in every mode
;; (show-paren-mode 1)
;; (setq show-paren-style 'mixed) ; Values; 'expression, 'parenthesis or 'mixed

;; Make matching parentheses and quotes always appear
;; https://github.com/capitaomorte/autopair
;; (require 'autopair)
;; (autopair-global-mode) ;; to enable in all buffers

;; smartparens
;; https://github.com/Fuco1/smartparens/
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode +1)
  )

;; Indenting
(electric-indent-mode 1)

;;;;
;;;; Editing
;;;;

;;;
;;; Markdown
;;;

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-hook 'markdown-mode-hook 'turn-on-outline-minor-mode)
  (add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)
  )

;;;
;;; Polymode, for Markdown + R + Yaml etc.
;;;

;;(use-package polymode
;;  :config
;;  (require 'poly-R)
;;  (require 'poly-markdown)
;;  (add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))
;;  (add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r+yaml-mode))
;;  )

;;;;
;;;; Faces 'n' fonts
;;;;

;; Measured in 0.1 pts
(setq wtd-font-height-for-this-machine 110)
(when (string= system-name "work")
  ;; Need it a little bigger on my work machine
  (setq wtd-font-height-for-this-machine 150))

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
(defun wtd/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (unless prefix
    (switch-to-next-buffer)))
(defun wtd/hsplit-last-buffer (prefix)
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
(setq x-select-enable-clipboard t)

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
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
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

;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only copy it
;; if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)
(eval-after-load 'latex
  '(define-key LaTeX-mode-map "\C-xn" nil))

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
(setq-default cursor-type 'bar) ;; Can be 'bar or 'box or '(hbar . 3) etc.

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

;; undo-tree-mode (trying out)
(global-undo-tree-mode)

;; aggressive-indent mode is aggressive indeed, but very handy.
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

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
(eval-after-load "aggressive-indent" '(diminish 'aggressive-indent-mode " →"))
(eval-after-load "anzu"              '(diminish 'anzu-mode))
(eval-after-load "auto-complete"     '(diminish 'auto-complete-mode " α"))
(eval-after-load "flymake"           '(diminish 'flymake-mode))
(eval-after-load "indent-guide"      '(diminish 'indent-guide-mode))
;;(eval-after-load "magit"             '(diminish 'magit-auto-revert-mode))
(eval-after-load "org-indent"        '(diminish 'org-indent-mode " →"))
(eval-after-load "rubocop"           '(diminish 'rubocop-mode))
(eval-after-load "ruby-block"        '(diminish 'ruby-block-mode))
(eval-after-load "simple"            '(diminish 'visual-line-mode))
(eval-after-load "smartparens"       '(diminish 'smartparens-mode " ()"))
(eval-after-load "subword"           '(diminish 'subword-mode))
(eval-after-load "undo-tree"         '(diminish 'undo-tree-mode " τ"))

;;;; powerline: Fancy up the modeline!
;; https://github.com/jonathanchu/emacs-powerline
;; Sticking with the default seems to be nice enough for me.

(require 'powerline)
(powerline-default-theme)

;;;;
;;;; visual-fill-column to give margins
;;;;

;; See http://www.lunaryorn.com/posts/center-buffer-text-in-emacs.html

(use-package visual-fill-column
  :defer t
  :bind (("C-c t v" . visual-fill-column-mode))
  :init
  (dolist (hook '(ruby-mode-hook)) ;; prog-mode-hook?
    (add-hook hook #'visual-fill-column-mode))
  :config (setq-default visual-fill-column-center-text t
                        visual-fill-column-fringes-outside-margins nil
			fill-column 120))

;;;;
;;;; ido
;;;;

;; See http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/

;; Flexible matching on filenames ... don't need to be exact.
(setq ido-enable-flex-matching t)

;; Go for it!  Everywhere!
(setq ido-everywhere t)

;; Turn it all one
(ido-mode 1)
(ido-ubiquitous-mode 1)

;; Shows options in a nice vertical list in the mini-buffer. Easier to
;; see.
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

;; Don't ask for confirmation if I create a new buffer with C-x b)
(setq ido-create-new-buffer 'always)

;; Customize the order of files shown in the minibuffer
(setq ido-file-extensions-order '(".org" ".rb" ".ini"))

;; Make ido use completion-ignored-extensions
(setq ido-ignore-extensions t)

;; Visit recently closed buffers with C-x b (works because recentf is
;; on)
(setq ido-use-virtual-buffers t)

;; Use cursor up and cursor down to move through the list
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; Let ido guess when find-file-at-point is what I want to do---if the
;; point is on a file path, then C-x C-f will open it.
(setq ido-use-filename-at-point 'guess)

;; Stop ido from switching into other directories when no match in
;; current one.
(setq ido-auto-merge-work-directories-length -1)

;; Save the state file in ~
(setq ido-save-directory-list-file "~/.ido.last")

;;;;
;;;; Smex works nicely with ido.
;;;;

;; Now that I'm using Counsel I don't want M-x bound for smex,
;; but Counsel does "piggyback on smex for sorting, if smex is installed,"
;; so keep it around.
;; (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;;;
;;;; flx and flx-ido for better fuzzy matching (https://github.com/lewang/flx)
;;;;

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;;;;
;;;; Ivy and Swiper
;;;;

;; https://github.com/abo-abo/swiper

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep) ;; I use C-c j for Jekyll
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;;;;
;;;; Projectile (https://github.com/bbatsov/projectile)
;;;;

(projectile-mode)

;; Key bindings: https://projectile.readthedocs.io/en/latest/usage/

;; Customize mode line
(setq projectile-mode-line '(:eval (format " ᴨ(%s)" (projectile-project-name))))

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
(define-key launcher-map "p" #'paradox-list-packages) ;; Or just 'list-packages if not using Paradox
(define-key launcher-map "s" #'eshell)
(define-key launcher-map "t" #'proced) ; top
(define-key launcher-map "u" #'magit-pull-from-upstream)
(define-key launcher-map "w" #'count-words-region)

;; Perhaps try launching other programs?
;; http://endlessparentheses.com/keymap-for-launching-external-applications-and-websites.html

;;;;
;;;; Zotero
;;;;

;; Zotxt
;; https://gitlab.com/egh/zotxt
;; https://github.com/egh/zotxt-emacs
;; See http://www.mkbehr.com/posts/a-research-workflow-with-zotero-and-org-mode/
;; In setup-org I turn on org-zotxt-mode in all Org files.

;;;;
;;;; Mode-specific customizations
;;;;

(require 'setup-autocomplete)
(require 'setup-elfeed)
(require 'setup-eshell)
(require 'setup-ess)
(require 'setup-expansions)
(require 'setup-file-management)
(require 'setup-ibuffer)
;; (require 'setup-javascript)
(require 'setup-jekyll)
(require 'setup-latex)
;; (require 'setup-markdown)
(require 'setup-orgmode)
(require 'setup-ruby)
(require 'setup-sonic-pi)
(require 'setup-useful-functions)
(require 'setup-yaml)
