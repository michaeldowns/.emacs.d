;; Generated settings
;; --------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-window-width 40)
 '(org-agenda-files (quote ("~/.notes.org")))
  '(send-mail-function (quote mailclient-send-it)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Monaco")))))

;; Run private routines
;; --------------------------------------
(let ((path "~/.emacs.d/init_private.el"))
  (if (and (file-exists-p path) (file-readable-p path))
      (load-file path)))

;; Load settings files
;; --------------------------------------
(add-to-list
 'load-path
 (expand-file-name "settings" user-emacs-directory))

(require 'my-package)
(require 'my-navigation)
(require 'my-code)
(require 'my-engine)
(require 'my-projects)
(require 'my-directory)
(require 'my-org)
(require 'language-javascript)
(require 'language-python)
(require 'language-sql)

;; Install packages if they're not already
;; --------------------------------------
(ensure-package-installed
 'neotree
 'which-key
 'better-defaults
 'exec-path-from-shell
 'yasnippet
 'super-save
 'restclient)

;; Enable global modes
;; --------------------------------------
(exec-path-from-shell-initialize)

(require 'super-save)
(super-save-initialize)

(global-hl-line-mode 1)

(which-key-mode 1)

;; Global settings
;; --------------------------------------
(setq-default
 which-key-idle-delay 0.2)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(setq-default
 inhibit-startup-screen t
 inhibit-startup-message t
 indent-tabs-mode nil
 auto-save-default nil
 make-backup-files nil)

(setq-default
 erc-lurker-hide-list '("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE")
 erc-lurker-threshold-time 14400)

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

;; use ssh for tramp
(setq tramp-default-method "ssh")

(column-number-mode 1)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Disable echoing shell
(defun my-comint-init ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'my-comint-init)

;; Default browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Utility functions
;; --------------------------------------

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))
