(require-package 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.8
      flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

(defun sigsegv/parse-elixir-dogma(output checker buffer)
  "Parse dogma errors from JSON OUTPUT.

CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://github.com/lpil/dogma' for more information
about dogma."
  (cl-flet ((alist-get (key alis) (cdr (assoc key alis))))
    (let* ((json-object-type 'alist)
           (json-array-type  'list)
           (dogma-json-output
            (car (alist-get 'files (json-read-from-string output))))
           (dogma-errors-list (alist-get 'errors dogma-json-output))
           (dogma-filename (alist-get 'path dogma-json-output))
           errors)
      (dolist (emessage dogma-errors-list)
        (let-alist emessage
          (push (flycheck-error-new-at
                 .line
                 1
                 'error .message
                 :id .rule
                 :checker checker
                 :buffer buffer
                 :filename dogma-filename)
                errors)))
      (nreverse errors))))

(after-load 'flycheck
  (diminish 'flycheck-mode " \u2714") ;; Checkmark
  (flycheck-define-checker elixir-dogma
    "An Elixir syntax checker using the Dogma analysis tool.

See URL `https://github.com/lpil/dogma/'."
    :command ("dogma" "--format=json" source)
    :error-parser sigsegv/parse-elixir-dogma
    :modes elixir-mode)
  (add-to-list 'flycheck-checkers 'elixir-dogma))

(provide 'init-flycheck)
