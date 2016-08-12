(ensure-package-installed
 'engine-mode)

(require 'engine-mode)
(engine-mode 1)

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")

(provide 'my-engine)
;;; my-engine.el ends here
