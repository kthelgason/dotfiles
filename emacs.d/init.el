
;; Init.el
(add-to-list 'load-path "~/.emacs.d/customizations")
(add-to-list 'load-path "~/.emacs.d/customizations/spaceline")
(require 'init-benchmarking) ;; Measure startup time
(setq gc-cons-threshold (* 128 1024 1024))

(require 'init-utils)
(require 'init-package)
(require 'init-exec-path)
(require 'init-functions)

(require-package 'scratch)
(require-package 'diminish)

(require 'init-evil)
(require 'init-frame-hooks)
(require 'init-theme)
(require 'init-keys)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-ido)
(require 'init-windows)
(require 'init-sessions)
(require 'init-fonts)
(require 'init-editing)
(require 'init-fci)
(require 'init-git)
(require 'init-ggtags)
(require 'init-paredit)
(require 'init-powerline)
(require 'init-projectile)

(require 'init-c)
(require 'init-compile)
(require 'init-markdown)
(require 'init-javascript)
(require 'init-web)
(require 'init-lisp)
(require 'init-clojure)
(require 'init-elixir)
(require 'init-ocaml)
(require 'init-sql)
(require 'init-latex)

(require 'init-misc)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;; Put customize code somewhere else
(setq custom-file "~/.emacs.d/emacs-customize.el")
(load custom-file)

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))

(provide 'init)
;;; init.el ends here








