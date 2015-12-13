(use-package js2-mode
  :ensure t
  :mode ("\\.js$" "\\.json$")
  :init
  (setq-default js2-basic-offset 2
                js2-bounce-indent-p nil
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  (add-hook 'js2-mode-hook (lambda ()
                             (setq mode-name "JS2")
                             (electric-indent-mode t))))

(provide 'setup-js)
