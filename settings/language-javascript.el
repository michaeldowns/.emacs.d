(ensure-package-installed
 'js2-mode
 'js2-refactor
 'tern
 'company-tern
 'json-mode)

(add-to-list
 'auto-mode-alist
 '("\\.js\\'" . js2-mode))

(setq-default
 js-indent-level 2
 js2-basic-offset 2
 ;; Supress js2 mode errors
 js2-mode-show-parse-errors nil
 js2-mode-show-strict-warnings)

(eval-after-load
    'flycheck
  (lambda ()
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    ;; Disable jshint
    (setq-default
     flycheck-disabled-checkers
     (append flycheck-disabled-checkers
	     '(javascript-jshint)))))

(defun my-javascript-mode-hook ()
  (js2-refactor-mode 1)
  (tern-mode 1)
  (flycheck-mode 1)
  (company-mode 1)
  (add-to-list 'company-backends 'company-tern))

(add-hook
 'js2-mode-hook
 'my-javascript-mode-hook)

(provide 'language-javascript)
;;; language-javascript.el ends here
