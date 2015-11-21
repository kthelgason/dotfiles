(use-package flycheck
             :ensure t
             :defer t
             :diminish (flycheck-mode . " \u2714") ;; Checkmark character
             :init
             ;; Turn flycheck on for all buffers
             (add-hook 'after-init-hook (lambda ()
                                          (global-flycheck-mode 1)
                                          (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))))

(provide 'setup-flycheck)

