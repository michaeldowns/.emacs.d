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

(provide 'my-org)
