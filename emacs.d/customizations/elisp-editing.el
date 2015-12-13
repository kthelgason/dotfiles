(defun elisp-keybindings ()
  (evil-define-key 'visual emacs-lisp-mode-map (kbd "C-x C-e") 'eval-region))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (rainbow-delimiters-mode t)
            (enable-paredit-mode)
            (eldoc-mode)
            (elisp-keybindings)
            (setq mode-name "ELisp")))


(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(setq-default initial-scratch-message
              (concat ";; Happy hacking " user-login-name " - Emacs ♥ you!\n\n"))
