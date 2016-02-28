(require-package 'clojure-mode)
(require-package 'cljsbuild-mode)
(require-package 'elein)

(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))

(after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'sanityinc/lisp-setup)
  (add-hook 'clojure-mode-hook 'subword-mode))

(require-package 'cider)

(setq nrepl-popup-stacktraces nil)

(after-load 'cider
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  ;; nrepl isn't based on comint
  (add-hook 'cider-repl-mode-hook
            (lambda () (setq show-trailing-whitespace nil))))

;; (require-package 'flycheck-clojure)
;; (after-load 'clojure-mode
;;   (after-load 'flycheck
;;     (flycheck-clojure-setup)))

(provide 'init-clojure)
