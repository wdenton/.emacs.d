;;; early-init.el --- Pre-initialization file for Emacs

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No startup screen
(setq inhibit-startup-message t)

;; Full screen!
(when (fboundp 'toggle-frame-maximized)
  (toggle-frame-maximized))

;; I don't want this
(setq site-run-file nil)

;; Don't garbage clean so often
(setq gc-cons-threshold 100000000)

(provide 'early-init)
