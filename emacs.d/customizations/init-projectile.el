(require-package 'projectile)

(dolist (mode '(c-mode-hook c++-mode-hook js2-mode-hook))
  (add-hook mode 'projectile-mode))

(setq projectile-enable-caching t)

(provide 'init-projectile)
