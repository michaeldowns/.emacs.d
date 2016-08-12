(ensure-package-installed
 'jinja2-mode
 'markdown-mode)

;; activate jinja2 when editing html
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq-default
 web-mode-markup-indent-offset 2
 web-mode-code-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.html\\'" . jinja2-mode))

(provide 'my-web)
