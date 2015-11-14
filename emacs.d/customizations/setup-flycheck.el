(use-package flycheck
             :ensure t
             :defer t
             :init
             ;; Turn flycheck on for all buffers
             (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'setup-flycheck)
