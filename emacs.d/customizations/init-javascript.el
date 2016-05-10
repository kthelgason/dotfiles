(require-package 'json-mode)
(require-package 'js2-mode)
(require-package 'ac-js2)

(defvar preferred-javascript-mode 'js2-mode)
(defconst preferred-javascript-indent-level 2)

(eval-when-compile (require 'cl))

(setq-default js2-basic-offset 2
              js2-bounce-indent-p nil)

(setq auto-mode-alist (cons `("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))
(after-load 'js2-mode
  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))

  (setq-default evil-shift-width 2)
  (after-load 'js2-mode
    (js2-imenu-extras-setup)))

(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))

(require-package 'rainbow-delimiters)
(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))

;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; ---------------------------------------------------------------------------

(require-package 'nodejs-repl)

(provide 'init-javascript)

