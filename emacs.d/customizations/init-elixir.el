(require-package 'elixir-mode)
(require-package 'ruby-end)
(require-package 'alchemist)

(defun sigsegv/activate-ruby-end-mode ()
  (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
       "\\(?:^\\|\\s-+\\)\\(?:do\\)")
  (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
  (ruby-end-mode +1)
  (diminish 'ruby-end-mode))

(defun sigsegv/alchemist-mode ()
  (alchemist-mode)
  (setq alchemist-test-status-modeline nil)
  (diminish 'alchemist-mode "⚗")
  (diminish 'alchemist-phoenix-mode))

(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))

(after-load 'elixir-mode
  (add-hook 'elixir-mode-hook 'sigsegv/alchemist-mode)
  (add-hook 'elixir-mode-hook 'sigsegv/activate-ruby-end-mode))


(provide 'init-elixir)
