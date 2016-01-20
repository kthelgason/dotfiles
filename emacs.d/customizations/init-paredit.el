(require-package 'paredit)
(require-package 'evil-paredit)
(autoload 'enable-paredit-mode "paredit")

(defun maybe-map-paredit-newline ()
  (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
              (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

(add-hook 'paredit-mode-hook 'maybe-map-paredit-newline)
(add-hook 'paredit-mode-hook 'evil-paredit-mode)

(after-load 'paredit
  (diminish 'paredit-mode " PE")
  (dolist (binding (list (kbd "C-<left>") (kbd "C-<right>")
                         (kbd "C-M-<left>") (kbd "C-M-<right>")))
    (define-key paredit-mode-map binding nil))

  ;; Modify kill-sentence, which is easily confused with the kill-sexp
  ;; binding, but doesn't preserve sexp structure
  (define-key paredit-mode-map [remap kill-sentence] 'paredit-kill)
  (define-key paredit-mode-map [remap backward-kill-sentence] nil)
  (evil-define-key 'normal paredit-mode-map (kbd "M-<right>") 'paredit-forward-slurp-sexp)
  (evil-define-key 'normal paredit-mode-map (kbd "M-<left>") 'paredit-forward-barf-sexp)
  (evil-define-key 'normal paredit-mode-map (kbd "M-<up>") 'paredit-backward-slurp-sexp)
  (evil-define-key 'normal paredit-mode-map (kbd "M-<down>") 'paredit-backward-barf-sexp)

  ;; Allow my global binding of M-? to work when paredit is active
  (define-key paredit-mode-map (kbd "M-?") nil))

;; ----------------------------------------------------------------------------
;; Enable some handy paredit functions in all prog modes
;; ----------------------------------------------------------------------------
(require-package 'paredit-everywhere)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)
(add-hook 'css-mode-hook 'paredit-everywhere-mode)
(after-load 'paredit-everywhere
  (define-key paredit-everywhere-mode-map [remap kill-sentence] 'paredit-kill))

(provide 'init-paredit)
