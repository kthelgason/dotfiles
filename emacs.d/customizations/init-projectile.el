(require-package 'projectile)

(projectile-global-mode 1)
(diminish 'projectile-mode)
(setq projectile-enable-caching t)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-completion-system 'ido)
(setq projectile-globally-ignored-directories
      '(".idea"
        ".eunit"
        ".git"
        ".hg"
        ".cache"
        ".fslckout"
        ".bzr"
        "_darcs"
        ".tox"
        ".svn"
        "build"
        "elpa"
        "node_modules"))

(after-load 'projectile
  (after-load 'evil
    (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)))

(provide 'init-projectile)
