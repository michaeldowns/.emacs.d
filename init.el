;; init.el --- Emacs configuration

;; Run private routines
;; --------------------------------------

(let ((path "~/.emacs.d/init_private.el"))
  (if (and (file-exists-p path) (file-readable-p path))
      (load-file path)))

;; Install packages if they're not already
;; --------------------------------------

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)

  (setq package-list '(paradox neotree buffer-move magit web-mode jinja2-mode
			       js2-mode flycheck json-mode auto-complete ac-js2
			       js2-refactor elpy py-autopep8 expand-region
			       multiple-cursors markdown-mode tern company-tern))

  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

;; Global settings
;; --------------------------------------

;; append /usr/local/bin to environment
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; append /usr/texbin to environment
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
(setq exec-path (append exec-path '("/usr/texbin")))

;; add brew to environment
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/homebrew/bin"))
(setq exec-path (append exec-path '("/usr/local/homebrew/bin")))

;; add python executables to environment
(setenv "PATH" (concat (getenv "PATH") ":~/Library/python/2.7/bin"))
(setq exec-path (append exec-path '("~/Library/python/2.7/bin")))

;; hide startup message
(setq inhibit-startup-message t)

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

(put 'erase-buffer 'disabled nil)

(put 'upcase-region 'disabled nil)

;; Disable echoing shell
(defun my-comint-init ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'my-comint-init)

;; Default browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Highlight the line after eighty characters
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; buffer-move commands
(global-set-key (kbd "<M-S-up>")     'buf-move-up)
(global-set-key (kbd "<M-S-down>")   'buf-move-down)
(global-set-key (kbd "<M-S-left>")   'buf-move-left)
(global-set-key (kbd "<M-S-right>")  'buf-move-right)

;; Enable windmove keybindings
;; Navigate to different windows by holding shift and using arrow keys
(windmove-default-keybindings)

;; FLycheck
;; --------------------------------------
(require 'flycheck)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(json-jsonlist)))


;; JavaScript / Web
;; --------------------------------------

;; disable jshint in flycheck
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'js2-mode)

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; enable flycheck
(add-hook 'js2-mode-hook 'flycheck-mode)

;; activate jinja2 when editing html
(add-to-list 'auto-mode-alist '("\\.html\\'" . jinja2-mode))

;; use two indents
(setq
 js-indent-level 2
 js2-basic-offset 2
 js2-mode-show-parse-errors nil
 js2-mode-show-strict-warnings)

(defun my-javascript-mode-hook ()
  (js2-refactor-mode 1)
  (tern-mode 1)
  (company-mode 1)
  (add-to-list 'company-backends 'company-tern))

(add-hook 'js2-mode-hook 'my-javascript-mode-hook)

;; Python
;; --------------------------------------

;; elpy
(elpy-enable)

;; autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; elpy flycheck
(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook 'flycheck-mode)

;; use jedi for elpy autocompletion
(setq elpy-rpc-backend "jedi")

;; Generated commands
;; --------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-window-width 40)
 '(org-agenda-files (quote ("~/.notes.org")))
 '(paradox-github-token t)
 '(send-mail-function (quote mailclient-send-it)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Monaco")))))

;; Utility functions
;; --------------------------------------

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))


;; Org mode
;; --------------------------------------

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

;; Record time when orgmode todo changed to done
(setq org-log-done 'time)

;; SQL
;; --------------------------------------

;; Postgres syntax highlighting
(add-to-list 'auto-mode-alist
             '("\\.psql$" . (lambda ()
                              (sql-mode)
                              (sql-highlight-postgres-keywords))))


;; Magit
;; --------------------------------------

;; Bind magit to C-x g
(global-set-key "\C-xg" 'magit-status)

;; multiple-cursors-mode
;; --------------------------------------

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; expand-region
;; --------------------------------------
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
