;; Run private routines
(let ((path "~/.emacs.d/init_private.el"))
  (if (and (file-exists-p path) (file-readable-p path))
      (load-file path)))

;; Generated commands
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/.notes.org")))
 '(paradox-github-token t)
 '(send-mail-function (quote mailclient-send-it)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Monaco")))))

;; use ssh
(setq tramp-default-method "ssh")

;; Add elisp folder to load path making it possible
;; to include files inside it if necessary
(add-to-list 'load-path "~/.emacs.d/elisp")

;; disable the toolbar and scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; line numbers
(global-linum-mode 1)
(column-number-mode 1)

;; remove backup file creation ability
(setq make-backup-files nil)

;; goto-line key binding
(global-set-key (kbd "C-c g") 'goto-line)

;; commenting and uncommenting regions
(global-set-key (kbd "C-c n") 'comment-region)
(global-set-key (kbd "C-c m") 'uncomment-region)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(put 'downcase-region 'disabled nil)

;; autopairing quotations etc.
(electric-pair-mode 1) ;; to enable in all buffers

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; Package management MELPA-Stable
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )
(put 'erase-buffer 'disabled nil)

(put 'upcase-region 'disabled nil)

;; Default browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Disable echoing shell
(defun my-comint-init ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'my-comint-init)

;; Highlight the line after eighty characters
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; append /usr/local/bin to environment
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; append /usr/texbin to environment
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
(setq exec-path (append exec-path '("/usr/texbin")))

;; add brew to environment
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/homebrew/bin"))
(setq exec-path (append exec-path '("/usr/local/homebrew/bin")))

;; orgmode settings
;; add additional workflow state
(setq org-todo-keywords
      '((sequence "TODO" "OPTIONAL" "|" "DONE")))

;; orgmode hotkeys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; org mode syntax highlighting
(setq org-src-fontify-natively t)

;; activate jinja2 when editing html
(add-to-list 'auto-mode-alist '("\\.html\\'" . jinja2-mode))

;; Record time when orgmode todo changed to done
(setq org-log-done 'time)

;; buffer-move commands
(global-set-key (kbd "<M-S-up>")     'buf-move-up)
(global-set-key (kbd "<M-S-down>")   'buf-move-down)
(global-set-key (kbd "<M-S-left>")   'buf-move-left)
(global-set-key (kbd "<M-S-right>")  'buf-move-right)

;; Enable windmove keybindings
;; Navigate to different windows by holding shift and using arrow keys
(windmove-default-keybindings)

;; Postgres syntax highlighting\
(add-to-list 'auto-mode-alist
             '("\\.psql$" . (lambda ()
                              (sql-mode)
                              (sql-highlight-postgres-keywords))))

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

