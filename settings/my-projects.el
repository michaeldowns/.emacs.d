;; settings related to coding projects in general

(ensure-package-installed
;; 'projectile
;; 'helm
;; 'helm-ag
;; 'helm-projectile
;; 'github-browse-file
 'magit)

;; Bind magit to C-x g
(global-set-key "\C-xg" 'magit-status)

;;; Enable projectile in all buffers
;;(add-hook 'after-init-hook 'projectile-global-mode)

;; Helmify everything
;; (helm-mode 1)
;; (setq-default
;;  helm-recentf-fuzzy-match 't
;;  helm-buffers-fuzzy-match 't
;;  helm-locate-fuzzy-match 't
;;  helm-M-x-fuzzy-match 't
;;  helm-imenu-fuzzy-match 't
;;  helm-apropos-fuzzy-match 't
;;  helm-lisp-completion-at-point 't)

;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)

;; (global-set-key (kbd "C-x g") 'magit-status)

;; (helm-projectile-on)

;; (setq-default
;;  projectile-completion-system 'helm
;;  vc-follow-symlinks 't)

(provide 'my-projects)
;;; my-projects.el ends here
