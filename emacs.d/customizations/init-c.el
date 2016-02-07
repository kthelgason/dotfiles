(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "bsd"))
      c-basic-offset 4)

(defun sigsegv/set-evil-shift-width ()
  "Set Evil's shift width for C editing."
  (after-load 'evil
    (setq evil-shift-width 4)))

(add-hook 'c-initialization-hook 'sigsegv/set-evil-shift-width)

(defun sigsegv/compile-and-run ()
  (interactive)
  (save-buffer)
  (shell-command (concat "clang++ --std=c++11 " (buffer-name) " && ./a.out")))

(defun sigsegv/c++-mode-setup ()
  "Setup C++-mode configurations."
  (interactive)
  (after-load 'flycheck
    (setq flycheck-clang-language-standard "c++11")
    (after-load 'projectile
      (if (projectile-project-root)
          (add-to-list
           'flycheck-clang-include-path
           (concat (projectile-project-root) "src")))
      )))

(add-hook 'c++-mode-hook 'sigsegv/c++-mode-setup)

(after-load 'evil
  (evil-define-key 'insert c-mode-map (kbd "TAB") 'c-indent-line-or-region)
  (evil-define-key 'insert c-mode-map (kbd "<backspace>") 'backward-delete-char-untabify)
  (evil-define-key 'normal c++-mode-map (kbd "s-r") 'sigsegv/compile-and-run)
  (evil-define-key 'insert c++-mode-map (kbd "<backspace>") 'backward-delete-char-untabify))

(provide 'init-c)
