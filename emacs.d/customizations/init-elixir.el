(require-package 'elixir-mode)
(require-package 'ruby-end)

(defun sigsegv/activate-ruby-end-mode ()
  (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
       "\\(?:^\\|\\s-+\\)\\(?:do\\)")
  (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
  (ruby-end-mode +1)
  (diminish 'ruby-end-mode))

(after-load 'elixir-mode
  (add-hook 'elixir-mode-hook 'sigsegv/activate-ruby-end-mode))


(provide 'init-elixir)
