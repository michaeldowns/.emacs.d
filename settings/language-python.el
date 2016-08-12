(ensure-package-installed
 'elpy
 'py-autopep8)

;; elpy
(elpy-enable)

;; use jedi for elpy autocompletion
(setq elpy-rpc-backend "jedi")

(require 'py-autopep8)

;; elpy flycheck
(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))

(defun my-python-mode-hook ()
  (flycheck-mode 1)
  (py-autopep8-enable-on-save))

(add-hook 'elpy-mode-hook 'my-python-mode-hook)

(provide 'language-python)
