(require-package 'ggtags)

(add-hook 'js2-mode-hook (lambda () (ggtags-mode 1)))

(provide 'init-ggtags)
