;; LaTeX

(require-package 'auctex)

(add-hook 'LaTeX-mode-hook
          (lambda () (push
                 '("Latexmk" "latexmk %s" TeX-run-TeX nil t
                   :help "Run Latexmk on file") TeX-command-list)))

(provide 'init-latex)
