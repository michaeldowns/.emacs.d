;; settings related to navigation in emacs

(ensure-package-installed
 ;; Navigation
 'ace-jump-mode
 'multiple-cursors
 'expand-region
 'buffer-move)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; goto-line key binding
(global-set-key (kbd "C-c g") 'goto-line)

;; Enable windmove keybindings
;; Navigate to different windows by holding shift and using arrow keys
(windmove-default-keybindings)

;; buffer-move commands
(global-set-key (kbd "<M-S-up>")     'buf-move-up)
(global-set-key (kbd "<M-S-down>")   'buf-move-down)
(global-set-key (kbd "<M-S-left>")   'buf-move-left)
(global-set-key (kbd "<M-S-right>")  'buf-move-right)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))
(global-set-key (kbd "C-a") 'smart-line-beginning)

(provide 'my-navigation)
