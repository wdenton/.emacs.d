;; DO NOT EDIT THIS FILE DIRECTLY
;; It is generated from an Org file.
;;
;; You should make any changes there and regenerate it with C-c C-v t

;; (setq debug-on-error t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities '(("melpa" . 20) ("gnu" . 10)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package diminish)
(use-package bind-key)

(server-mode)

(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  redefine
                                  obsolete
                                  noruntime
                                  cl-functions
                                  interactive-only
                                  ))

(setq custom-file "~/.custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(use-package async
  :defer t
  :config
  (dired-async-mode 1)
  )

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  )

(set-default 'sentence-end-double-space nil)

(setq calendar-week-start-day 1)

;; (fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

(setq echo-keystrokes 0.1)

(setq use-dialog-box nil)

(global-visual-line-mode 1)

(global-hl-line-mode 1)
;; And set its colour
;; (set-face-background hl-line-face "#efefef")

(set-default 'cursor-type '(bar . 2))

(setq wtd-ubuntu-mono-height
      (cond ((string= (system-name) "marcus") 120)
	    ((string= (system-name) "ochre") 140)
	    ((string= (system-name) "work") 160)
	    (t 120))
      )

(set-face-attribute 'default nil :font "Ubuntu Mono" :height wtd-ubuntu-mono-height)
;; (set-face-attribute 'default nil :font "Fira Code" :height wtd-fira-code-height)
;; (set-face-attribute 'variable-pitch nil :family "Baskervald ADF Std" :height wtd-font-height)

(setq inhibit-compacting-font-caches t)

(setq font-lock-maximum-decoration t)

(use-package list-unicode-display)

(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(show-paren-mode t)
(setq blink-matching-paren t)

(setq tab-width 4)

(use-package aggressive-indent
  :diminish aggressive-indent-mode ;; "→"
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  )

(use-package solarized-theme
  :config
  ;; Stop the theme from messing up Org headlines and using variable pitch everywhere.
  (setq solarized-use-variable-pitch nil
	solarized-scale-org-headlines nil)
  (setq x-underline-at-descent-line t) ;; Try this out.
  (load-theme 'solarized-dark t)
  )

(size-indication-mode t)

(column-number-mode t)

(use-package powerline
  :config
  (powerline-default-theme)
  )

(diminish 'abbrev-mode)
(diminish 'emacs-lisp-d-mode " Ⓛ")

(eval-after-load "autorevert" '(diminish 'auto-revert-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
;; (eval-after-load "flymake" '(diminish 'flymake-mode))
(eval-after-load "flyspell" '(diminish 'flyspell-mode ""))
;;(eval-after-load "magit" '(diminish 'magit-auto-revert-mode))
(eval-after-load "org-indent" '(diminish 'org-indent-mode)) ;; →
(eval-after-load "outline" '(diminish 'outline-minor-mode))
(eval-after-load "rainbow-mode" '(diminish 'rainbow-mode))
(eval-after-load "simple" '(diminish 'visual-line-mode))
(eval-after-load "smerge-mode" '(diminish 'smerge-mode))
(eval-after-load "subword" '(diminish 'subword-mode))

(global-prettify-symbols-mode 1)

(setq prettify-symbols-unprettify-at-point 'right-edge)

(setq-default prettify-symbols-alist '(("->" . ?→)
 				       ("<-" . ?←)
                                       ("->>" . ?↠)
                                       ("->>" . ?↞)
                                       ("=>" . ?⇒)
                                       ("!=" . ?≠)
                                       ("==" . ?≡)
                                       ("<=" . ?≤)
                                       (">=" . ?≥)
				       ))

(put 'narrow-to-region 'disabled nil)

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

(use-package wrap-region
  :defer t
  :diminish wrap-region-mode
  :config
  ;; (wrap-region-mode t)
  (wrap-region-add-wrappers
   '(("*" "*" nil org-mode)
     ("~" "~" nil org-mode)
     ("/" "/" nil org-mode)
     ("=" "=" "+" org-mode)
     ("_" "_" nil org-mode)
     ("$" "$" nil (org-mode latex-mode))))
  :init
  (add-hook 'org-mode-hook 'wrap-region-mode)
  (add-hook 'latex-mode-hook 'wrap-region-mode)
  )

(use-package expand-region
  :defer t
  :init
  (global-set-key (kbd "C-=") 'er/expand-region)
  )

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

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(use-package projectile
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-mode-line-function '(lambda () (format " ᴨ[%s]" (projectile-project-name))))
  )

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "")

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

(defun wtd/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'wtd/kill-this-buffer)

(defalias 'list-buffers 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
 	       ("dired" (mode . dired-mode))
 	       ("emacs" (or
 			 (name . "^\\*scratch\\*$")
 			 (name . "^\\*Messages\\*$")))
	       ))))

(use-package ibuffer-projectile
  :defer t
  :init
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (ibuffer-projectile-set-filter-groups)
	      (unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic))))
  )

(setq split-height-threshold nil)
(setq split-width-threshold 0)

(defun wtd/vsplit-last-buffer (PREFIX)
  "Split the window vertically and display the previous buffer.
   By default, switch to that new window; with PREFIX, stay where you are."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (unless prefix
    (switch-to-next-buffer)))

(defun wtd/hsplit-last-buffer (PREFIX)
  "Split the window horizontally and display the previous buffer.
   By default, switch to that new window; with PREFIX, stay where you are."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (unless prefix (switch-to-next-buffer)))

(global-set-key (kbd "C-x 2") 'wtd/vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'wtd/hsplit-last-buffer)

(winner-mode t)

(global-set-key (kbd "M-o") 'other-window)

(require 'saveplace)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))
(save-place-mode)

(use-package amx
  :requires helm
  :after ivy
  :custom
  (amx-backend 'ivy)
  )

(use-package counsel
  ;; :bind (("C-x b" . counsel-switch-buffer) ;; Show list of buffers to switch to, but also show the buffer at point while moving through list.
  ;; I stopped doing this because it slows down on large Org files, and I don't really need it.
	 )
  )

(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  )

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
	     ("M-i" . counsel-imenu)
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
  )

(define-prefix-command 'launcher-map)
(define-key ctl-x-map "l" 'launcher-map)
(define-key launcher-map "c" #'calculator) ; calc is too much
(define-key launcher-map "g" #'magit-status)
(define-key launcher-map "l" #'goto-line)
(define-key launcher-map "m" #'mc/edit-lines)
(define-key launcher-map "p" #'list-packages)
(define-key launcher-map "s" #'eshell)
(define-key launcher-map "u" #'magit-pull-from-upstream)
(define-key launcher-map "w" #'count-words-region)

(setq scroll-conservatively 10000)

(setq auto-window-vscroll nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq require-final-newline t)

(setq next-line-add-newlines nil)

(set-default 'indicate-empty-lines t)

(use-package rainbow-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode)
  :config
  (rainbow-mode t) ;; #0af
  )

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

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

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

(setq case-fold-search nil)

(setq search-highlight t)

(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode t)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
  )

(setq max-specpdl-size 50000)
(setq max-lisp-eval-depth 25000)

(transient-mark-mode t)

(delete-selection-mode 1)

(use-package volatile-highlights
  :init (volatile-highlights-mode t)
  :diminish volatile-highlights-mode
  :config
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  )

(global-set-key [(shift delete)] 'clipboard-kill-region)
(global-set-key [(control insert)] 'clipboard-kill-ring-save)
(global-set-key [(shift insert)] 'clipboard-yank)

(setq select-enable-clipboard t)

(global-set-key (kbd "C-<backspace>") (lambda ()
					(interactive)
					(kill-line 0)))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(setq-default abbrev-mode t)

(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode 1)
  )

(setq tab-always-indent 'complete) ;; Try to indent current line; if already indented, try to complete the thing at point.

(use-package corfu
  :init
  (global-corfu-mode)
  )

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil) ;; If t then big dot files start accumulating
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  )

(require 'recentf)
(recentf-mode 1)
(setq recentf-save-file "~/.recentf")

(global-set-key (kbd "C-x f") 'find-file-at-point)

(global-auto-revert-mode t)

(defadvice completion--file-name-table (after ignoring-backups-f-n-completion activate)
  "Filter out results when they match `completion-ignored-extensions'."
  (let ((res ad-return-value))
    (if (and (listp res)
	     (stringp (car res))
	     (cdr res))                 ; length > 1, don't ignore sole match
	(setq ad-return-value
              (completion-pcm--filename-try-filter res)))))

(setq global-auto-revert-non-file-buffers t)

(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'" "evince")
	("\\.tex\\'" "pdflatex")
	("\\.ods\\'\\|\\.xlsx?\\'\\|\\.docx?\\'\\|\\.csv\\'" "libreoffice")))

(require 'dired-x)

(setq diredp-hide-details-initially-flag nil)

(put 'dired-find-alternate-file 'disabled nil)

(setq dired-kill-when-opening-new-dired-buffer t)

(setq dired-dwim-target t)

(setq font-lock-maximum-decoration (quote ((dired-mode) (t . t))))

(defun delete-current-buffer-file ()
  "Delete file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun rename-current-buffer-file ()
  "Rename current buffer and file it is visiting."
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

(use-package csv-mode)

(use-package json-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (
	 ("README\\.md\\'" . gfm-mode)
	 )
  :init
  ;; (setq markdown-command "multimarkdown")
  (setq markdown-hide-urls t
	markdown-hide-markup t
	markdown-url-compose-char "⋯"
	markdown-header-scaling t
	)
  :hook
  'turn-on-visual-line-mode
  )

(add-to-list 'auto-mode-alist
	     (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
		   'nxml-mode))

(add-to-list 'hs-special-modes-alist
	     '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"
               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)

(with-eval-after-load "nxml-mode"
  (define-key nxml-mode-map "\C-c h" 'hs-toggle-hiding))

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  )

(use-package magit
  ;; :config
  )

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t)
  (setq git-gutter-fr:side 'right-fringe)
  )

(use-package org
  ;; Use Org's current development branch, pulled down with Git.  See https://orgmode.org/org.html#Installation.
  :pin manual
  :load-path "/usr/local/src/org-mode/lisp"
  :init
  (setq
    org-fontify-whole-heading-line t
    org-pretty-entities t ;; org-entities displays \alpha etc. as Unicode characters.
    org-hide-emphasis-markers t ;; Hide the /italics/ and *bold* markers
    org-hide-macro-markers t ;; Hide {{{macro}}} curly brackets; see also wtd/toggle-org-macro-markers
    org-return-follows-link t ;; Hit return on a link to open it in a browser
    org-support-shift-select t ;; Shift and arrow keys to select text works a bit differently in Org.
    org-special-ctrl-a/e t ;; Make C-a and C-e understand how headings and tags work
    org-startup-indented t ;; Visually indent everything nicely, but leave the raw file left-aligned
    org-cycle-separator-lines 0 ;; Never show blank lines in condensed view
    org-src-fontify-natively t ;; Fontify Babel blocks nicely
    org-src-preserve-indentation t ;; Preserve indentation when tangling source blocks (important for makefiles)
    org-list-allow-alphabetical t ;; Allow a b c lists
    org-use-speed-commands t ;; Allow speed commands
    org-tags-column 120 ;; Right-align tags to an indent from the right margin, could use  (- 50 (window-width))
    org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar" ;; External dependency: ditaa
    org-display-inline-images t ;; Embed an image with [[file:foo.png]] and then C-c C-x C-v to view
    org-startup-with-inline-images t ;; Show images on startup
    org-image-actual-width nil ;; nil means "try to get the width from an #+ATTR.* keyword and fall back on the original width if none is found."
    org-highlight-latex-and-related '(latex) ;; Highlight inline LaTeX
    org-export-with-smart-quotes t ;; Turn plain quotes into posh (I can't include examples in here or it breaks paren matching!)
    org-confirm-babel-evaluate nil ;; Evaluate Babel blocks without asking for confirmation
    org-ellipsis " ⬎" ;; ⤵ ↴  Change the ellipsis that indicates hidden content
    org-footnote-section nil ;; Define footnotes nearby when I use C-c C-x f
    org-footnote-auto-adjust nil ;; Don't resort or adjust them without my saying so.
    org-export-backends (quote (html latex md odt beamer)) ;; Exporting: I will see these export options after C-c C-e ;; beamer reveal
    org-src-window-setup 'current-window ;; How to rearrange things when I edit a source block.  Default is regorganize-frame.
    ;; org-export-date-timestamp-format "%d %m %Y" ;; Date format on exports
    )

  ;; Define my own link abbreviations
  (setq org-link-abbrev-alist
	'(
	  ("DOI" . "http://dx.doi.org/%s")                        ;; Thus [[DOI:10.1108/07378831111138189]]
	  ("WP"  . "https://en.wikipedia.org/wiki/%s")            ;; Thus [[WP:Toronto, Ontario]]
	  ("YUL" . "https://www.library.yorku.ca/find/Record/%s") ;; Thus [[YUL:2935857]]
	  )
	)

  ;; Clocking
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; Automatically refresh inline images that are generated from Babel blocks
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  ;; Display images when a file is loaded (I can always toggle them off if I don't want them)
  (add-hook 'org-mode-hook (lambda () (org-toggle-inline-images)))

  ;; Use LaTeX spell-check
  (add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))

  ;; Use C-c d to close all the open drawers in a file
  (defun add-org-close-all-drawers-key ()
    (local-set-key (kbd "C-c d") (lambda () (interactive) (org-cycle-hide-drawers 'all))))
  (add-hook 'org-mode-hook 'add-org-close-all-drawers-key)

  ;; Hooks for prettify-symbols-mode
  ;; See also https://pank.eu/blog/pretty-babel-src-blocks.html for some cool stuff
  ;; And https://github.com/zzamboni/dot-emacs/blob/master/init.org#source-code-blocks
  ;; for some stuff I tried out but decided was a bit too much for me.
  (add-hook 'org-mode-hook
 	  (lambda ()
 	    (push '("<=" . ?≤) prettify-symbols-alist)
 	    (push '(">=" . ?≥) prettify-symbols-alist)
 	    (push '("|>" . ?▷) prettify-symbols-alist)
 	    (push '("#+BEGIN_SRC" . ?⎡) prettify-symbols-alist) ;;  ⎡ ➤ ➟ ➤ ✎
 	    (push '("#+END_SRC" . ?⎣) prettify-symbols-alist) ;; ⎣ ✐
 	    (push '("#+begin_src" . ?⎡) prettify-symbols-alist)
 	    (push '("#+end_src" . ?⎣) prettify-symbols-alist)
 	    (push '("#+BEGIN_QUOTE" . ?❝) prettify-symbols-alist)
 	    (push '("#+END_QUOTE" . ?❞) prettify-symbols-alist)
 	    (push '("#+begin_quote" . ?❝) prettify-symbols-alist)
 	    (push '("#+end_quote" . ?❞) prettify-symbols-alist)
 	    ;; (push '("[ ]" . ?☐) prettify-symbols-alist)
 	    ;; (push '("[X]" . ?☒) prettify-symbols-alist)
 	    ))

  :config
  (global-set-key "\C-cl" 'org-store-link)

  ;; In 25 Org started opening exported PDFs in docview, but I prefer seeing them externally.
  ;; (delete '("\\.pdf\\'" . default) org-file-apps)
  ;; (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))

  ;; Active Babel languages (http://orgmode.org/org.html#Languages)
  (org-babel-do-load-languages 'org-babel-load-languages '(
     (ditaa . t)
     (dot . t)
     (latex . t)
     (lilypond . t)
     (python . t)
     (R . t)
     (ruby . t)
     (shell . t)
     (sql . t)
     (sqlite . t)
     ))

  ;; Appearance.  This should probably go elsewhere.

  (set-face-attribute 'org-link nil :foreground "Steel Blue")
  (set-face-attribute 'org-footnote nil :height 0.9)

  ;; Make completed items in a checkbox list less noticeable
  ;; https://fuco1.github.io/2017-05-25-Fontify-done-checkbox-items-in-org-mode.html
  (font-lock-add-keywords
   'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
 'append)

  ;; Source code block appearance
  (set-face-attribute 'org-block-begin-line nil :underline nil)
  (set-face-attribute 'org-block-end-line nil :overline nil)

  (set-face-attribute 'org-verbatim nil :family "Ubuntu Mono" :height wtd-ubuntu-mono-height)

  (set-face-attribute 'org-ellipsis nil :underline nil)

  ;; Make LOGBOOK and such fainter.  Default bold is too loud.
  (face-spec-set 'org-drawer '((t (:foreground "dim gray" :weight normal))))

  ;; (face-spec-set 'org-level-1 '((t (:height 1.05))))
  ;; (face-spec-set 'org-level-2 '((t (:height 1.05))))
  ;; (face-spec-set 'org-level-3 '((t (:height 1.0))))

  :hook
  (
   (org-mode . wrap-region-mode)
   (org-mode . turn-on-flyspell) ;; Use flyspell to check spelling as I go
   )
  )

(use-package org-appear
  :config
  :hook (org-mode . org-appear-mode)
  )

(defun wtd/toggle-org-macro-markers ()
  "Toggle visibility of {{{macro}}} markers"
  (interactive)
  (setq org-hide-macro-markers (not org-hide-macro-markers))
  (font-lock-mode)
  (font-lock-mode))

(eval-after-load 'org-src
  '(define-key org-src-mode-map
     "\C-x\C-s" #'org-edit-src-exit))

(defun wtd/org-link-delete-link ()
  "Remove the link from an Org link at point and keep only the description."
  (interactive)
  (let ((elem (org-element-context)))
    (if (eq (car elem) 'link)
        (let* ((content-begin (org-element-property :contents-begin elem))
               (content-end  (org-element-property :contents-end elem))
               (link-begin (org-element-property :begin elem))
               (link-end (org-element-property :end elem)))
          (if (and content-begin content-end)
              (let ((content (buffer-substring-no-properties content-begin content-end)))
                (delete-region link-begin link-end)
                (insert content)))))))

(defun org-toggle-src-eval-no ()
  "Toggle ':eval no' on the src block begin line."
  (defun in-src-block-p ()
    "Returns t when the point is inside a source code block"
    (string= "src" (org-in-block-p '("src"))))
  (defun beginning-src ()
    "Find the beginning of the src block"
    (let ((case-fold-search t)) (search-backward "#+BEGIN_SRC")))
  (defun toggle-eval-no ()
    "Handles the toggling of ' :eval no'"
    (save-excursion
      (end-of-line)
      (let ((case-fold-search t)) (search-backward "#+BEGIN_SRC")
	   (if (search-forward " :eval no" (line-end-position) "f")
	       (replace-match "")
	     (insert " :eval no")
	     ))))
  (if (in-src-block-p) (toggle-eval-no)))
(defun add-org-toggle-src-key ()
  (local-set-key (kbd "C-c t") (lambda () (interactive) (org-toggle-src-eval-no))))
(add-hook 'org-mode-hook 'add-org-toggle-src-key)

(defun wtd/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+begin_src" . "^#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE ". "#\\+END_EXAMPLE"))
  )
(add-hook 'org-mode-hook #'wtd/org-ispell)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("memoir-blocked"
		 "\\documentclass{memoir}
	       [NO-DEFAULT-PACKAGES]
	       [NO-PACKAGES]
	       [NO-EXTRA]"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("letter" "\\documentclass{letter}"))
  (add-to-list 'org-latex-classes
               '("memoir"
                 "\\documentclass{memoir}"
                 ("\\book{%s}" . "\\book*{%s}")
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("memoir-chapter+"
                 "\\documentclass{memoir}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  )

(use-package org-superstar
  :config
  (setq org-superstar-headline-bullets-list '("⊢" "ǁ" "ǀ" "⋱" "◉" "○")) ;; ǂ ⋮
  :hook
  (org-mode . (lambda () (org-superstar-mode 1)))
  )

(add-hook 'prog-mode-hook 'subword-mode)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq ansi-color-for-comint-mode 'filter
      comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output t
      comint-move-point-for-output t
 )
;;(setq comint-prompt-read-only t)

(setq ansi-color-for-comint-mode 'filter)

(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
  (setq flycheck-global-modes '(not org-mode)) ;; Could also set :modes to list where I want it.
  )

(setq-default flycheck-lintr-linters
              (concat "with_defaults(line_length_linter(120), "
                      "absolute_paths_linter = NULL, "
		      ;; "camel_case_linter = NULL, "
		      ;; "snake_case_linter = NULL, "
		      "commented_code_linter = NULL)"))

(use-package ess
  :config
  ;; (setq ess-smart-S-assign-key nil) ;; Don't let _ turn into <-
  ;; (setq ess-use-auto-complete t) ;; Auto-completion on.
  (setq ess-use-flymake nil ;; Don't run flymake on ESS buffers
	ess-help-own-frame 'nil ;; Make all help buffers go into one frame
	;; inferior-ess-primary-prompt "> "
	 ess-can-eval-in-background nil ;; Added Feb 2022, wait for ESS to catch up with Emacs ()see https://stat.ethz.ch/pipermail/ess-help/2022-February/013031.html)
	)
  ;; (setq inferior-S-prompt "[]a-zA-Z0-9.[]*\\(?:[>+.] \\)*ℝ+> ")
  :init
  (add-hook 'ess-R-post-run-hook 'ess-execute-screen-options) ;; Use the full width of the Emacs frame
  (add-hook 'ess-post-run-hook 'ess-execute-screen-options)
  (add-hook 'ess-mode-hook 'ess-execute-screen-options)
  ;; (add-hook 'ess-mode-hook 'highlight-indent-guides-mode)   ;; indent-guide ... very nice
  ;; (add-hook 'ess-mode-hook (lambda () (flycheck-mode t)))
  )

(setq ess-ask-for-ess-directory nil)

(add-hook 'inferior-ess-mode-hook
	  (lambda ()
	    (push '("%>%" . ?|) prettify-symbols-alist)
 	    (push '("|>" . ?▷) prettify-symbols-alist)
	    ))

(add-hook 'ess-mode-hook
	  (lambda ()
	    (push '("%>%" . ?|) prettify-symbols-alist)
 	    (push '("|>" . ?▷) prettify-symbols-alist)
	    ))

(setq ess-local-process-name "R")

(add-hook 'ess-mode-hook
	  (lambda ()
	    (setq ess-indent-offset 4
		  ess-offset-continued 2
		  ess-offset-continued 'straight
		  ess-brace-offset -4
		  ess-expression-offset 4
		  ess-else-offset 0
		  ess-close-brace-offset 0
		  ess-brace-imaginary-offset 0
		  ess-continued-brace-offset 0
		  ess-indent-from-lhs 4
		  ess-offset-arguments-newline '(4)
	    )))

(setq ess-R-font-lock-keywords
      (quote
       ((ess-R-fl-keyword:modifiers . t)
        (ess-R-fl-keyword:fun-defs . t)
        (ess-R-fl-keyword:keywords . t)
        (ess-R-fl-keyword:assign-ops . t)
        (ess-R-fl-keyword:constants . t)
        (ess-fl-keyword:fun-calls . t)
        (ess-fl-keyword:numbers . t)
        (ess-fl-keyword:operators . t)
        (ess-fl-keyword:delimiters . t)
        (ess-fl-keyword:=)
	(ess-R-fl-keyword:F&T))))

(add-hook 'ruby-mode-hook
	  (lambda ()
	    (push '("!=" . ?≠) prettify-symbols-alist)
	    ;; (push '("&&" . ?∧) prettify-symbols-alist)
 	    ;; (push '("||" . ?∨) prettify-symbols-alist)
	    ))

(use-package rbenv
  :hook (ruby-mode . global-rbenv-mode)
  :config
  (setq rbenv-show-active-ruby-in-modeline nil
	rbenv-modeline-function 'rbenv--modeline-plain
   )
  (rbenv-use-global)
  )

(use-package inf-ruby
  :hook (ruby-mode . inf-ruby-minor-mode)
  :config
  (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
  (add-to-list 'inf-ruby-implementations' ("pry". "pry"))
  (setq inf-ruby-default-implementation "pry")
  )

(setq ruby-deep-indent-paren nil)

(setq ruby-insert-encoding-magic-comment nil)

(add-hook 'ruby-mode-hook #'goto-address-mode)

(use-package rubocop
  :diminish rubocop-mode
  :hook (ruby-mode . rubocop-mode)
  )

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

(global-set-key (kbd "C-c j n") 'jekyll-draft-post)
(global-set-key (kbd "C-c j p") 'jekyll-publish-post)
(global-set-key (kbd "C-c j t") 'jekyll-timestamp)
(global-set-key (kbd "C-c j o") (lambda () (interactive) (find-file "~/web/")))

(defvar jekyll-directory "~/web/" "Path to Jekyll blog.")
(defvar jekyll-drafts-dir "_drafts/" "Relative path to drafts directory.")
(defvar jekyll-posts-dir "_posts/" "Relative path to posts directory.")
(defvar jekyll-post-ext ".md"  "File extension of Jekyll posts.")
(defvar jekyll-post-template "---\nlayout: post\ntitle: %s\ntags:\ndate: \n---\n"
  "Default template for Jekyll posts. %s will be replace by the post title.")

(defun jekyll-make-slug (s) "Turn string S into a slug."
       (replace-regexp-in-string " " "-"  (downcase (replace-regexp-in-string "[^A-Za-z0-9 ]" "" s))))

(defun jekyll-yaml-escape (s) "Escape string S for YAML."
       (if (or (string-match ":" s) (string-match "\"" s)) (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"") s))

(defun jekyll-draft-post (title) "Create a new Jekyll blog post with title TITLE."
       (interactive "sPost Title: ")
       (let ((draft-file (concat jekyll-directory jekyll-drafts-dir
				 (jekyll-make-slug title)
				 jekyll-post-ext)))
	 (if (file-exists-p draft-file)
             (find-file draft-file)
	   (find-file draft-file)
	   (insert (format jekyll-post-template (jekyll-yaml-escape title))))))

(defun jekyll-publish-post () "Move a draft post to the posts directory, and rename it to include the date."
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

(add-hook 'LaTeX-mode-hook #'outline-minor-mode)

(use-package auctex
  :defer t
  :diminish auctex
  )

(add-hook 'LaTeX-mode-hook 'turn-on-visual-line-mode)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(setq latex-run-command "pdflatex")
;; Use pdflatex to make PDFs
;; For some reason this value isn't respected and I had to set
;; it through Custom. Don't know why.
;; TEMP
(setq TeX-PDF-mode t)
; (customize-set-variable 'org-latex-pdf-process '("latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))

(setq revert-without-query '(".pdf"))

(setq biblatex-dialect "biblatex")

(add-hook 'TeX-mode-hook (lambda () (TeX-fold-mode 1)))

(add-hook 'latex-mode-hook 'wrap-region-mode)

(setq LaTeX-item-indent 0)

(put 'LaTeX-narrow-to-environment 'disabled nil)
(put 'TeX-narrow-to-group 'disabled nil)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(eval-after-load "reftex" '(diminish 'reftex-mode))
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

(eval-after-load 'reftex-vars
  '(progn
     ;; (also some other reftex-related customizations)
     (setq reftex-cite-format
           '((?\C-m . "\\cite[]{%l}")
             (?f . "\\footcite[][]{%l}")
             (?t . "\\textcite[]{%l}")
             (?p . "\\parencite[]{%l}")
             (?o . "\\citepr[]{%l}")
             (?n . "\\nocite{%l}")))))
