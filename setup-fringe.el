;;; Taken from Bastien Guerry
;;; http://bzg.fr/emacs-strip-tease.html

;; A small minor mode to use a big fringe
(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2))))

;; Set the color of the fringe
(custom-set-faces
 '(fringe ((t (:background "#002b36"))))) ; base03 from Solarized dark

;; Now activate this global minor mode
(bzg-big-fringe-mode 1)

;; To find the background colour
;; (face-attribute 'default :background)

;; To activate the fringe by default and deactivate it when windows
;; are split vertically, uncomment this:
(add-hook 'window-configuration-change-hook
	  (lambda ()
	    (if (delq nil
		      (let ((fw (frame-width)))
			(mapcar (lambda(w) (< (window-width w) fw))
				(window-list))))
		(bzg-big-fringe-mode 0)
	      (bzg-big-fringe-mode 1))))

;; Get rid of the indicators in the fringe
(mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
        fringe-bitmaps)

(provide 'setup-fringe)
