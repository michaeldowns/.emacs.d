;; Settings related to programming modes
;; --------------------------------------

(ensure-package-installed
 'flycheck
 'diff-hl
 'rainbow-delimiters
 'company)

;; commenting and uncommenting regions
(global-set-key (kbd "C-c n") 'comment-region)
(global-set-key (kbd "C-c m") 'uncomment-region)

;; Company mode
(add-hook
 'company-mode-hook
 '(lambda ()
    ;; Slightly better autocomplete on tab
    (define-key company-active-map [tab] 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)))

(setq-default
 company-idle-delay 0.1
 company-minimum-prefix-length 1)

(require 'flycheck)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(json-jsonlist)))

(add-hook
 'prog-mode-hook
 'my-code-mode-init)

;; Highlight the line after eighty characters
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(defun my-code-mode-init ()
  (linum-mode 1) 
  (rainbow-delimiters-mode 1)
  (diff-hl-mode 1)
  (show-paren-mode 1)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(provide 'my-code)
