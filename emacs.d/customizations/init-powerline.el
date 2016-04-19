(require-package 'powerline)
(require-package 'powerline-evil)

(require 'spaceline-config)

(spaceline-spacemacs-theme)
(setq spaceline-highlight-face-func
      'spaceline-highlight-face-evil-state)

(provide 'init-powerline)
