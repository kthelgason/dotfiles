(require-package 'elisp-slime-nav)
(after-load 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELisp")))

(require-package 'lively)

(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

;; Make C-x C-e run 'eval-region if the region is active
(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(global-set-key (kbd "M-:") 'pp-eval-expression)

(after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'sanityinc/eval-last-sexp-or-region))

(require-package 'ipretty)
(ipretty-mode 1)


(defadvice pp-display-expression (after sanityinc/make-read-only (expression out-buffer-name) activate)
  "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
  (when (get-buffer out-buffer-name)
    (with-current-buffer out-buffer-name
      (view-mode 1))))

(defun sanityinc/maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode 1)))

(add-hook 'emacs-lisp-mode-hook 'sanityinc/maybe-set-bundled-elisp-readonly)

;; Use C-c C-z to toggle between elisp files and an ielm session
;; I might generalise this to ruby etc., or even just adopt the repl-toggle package.

(defvar sanityinc/repl-original-buffer nil
  "Buffer from which we jumped to this REPL.")
(make-variable-buffer-local 'sanityinc/repl-original-buffer)

(defvar sanityinc/repl-switch-function 'switch-to-buffer-other-window)

(defun sanityinc/switch-to-ielm ()
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (if (get-buffer "*ielm*")
        (funcall sanityinc/repl-switch-function "*ielm*")
      (ielm))
    (setq sanityinc/repl-original-buffer orig-buffer)))

(defun sanityinc/repl-switch-back ()
  "Switch back to the buffer from which we reached this REPL."
  (interactive)
  (if sanityinc/repl-original-buffer
      (funcall sanityinc/repl-switch-function sanityinc/repl-original-buffer)
    (error "No original buffer.")))

(after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'sanityinc/switch-to-ielm))
(after-load 'ielm
  (define-key ielm-map (kbd "C-c C-z") 'sanityinc/repl-switch-back))

;; ----------------------------------------------------------------------------
;; Automatic byte compilation
;; ----------------------------------------------------------------------------
(when (maybe-require-package 'auto-compile)
  (auto-compile-on-save-mode 1)
  (auto-compile-on-load-mode 1))

;; ----------------------------------------------------------------------------
;; Load .el if newer than corresponding .elc
;; ----------------------------------------------------------------------------
(setq load-prefer-newer t)

;; ----------------------------------------------------------------------------
;; Highlight current sexp
;; ----------------------------------------------------------------------------

(require-package 'hl-sexp)

;; Prevent flickery behaviour due to hl-sexp-mode unhighlighting before each command
(after-load 'hl-sexp
  (defadvice hl-sexp-mode (after unflicker (&optional turn-on) activate)
    (when turn-on
      (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))

;;; Support byte-compilation in a sub-process, as
;;; required by highlight-cl

(defun sanityinc/byte-compile-file-batch (filename)
  "Byte-compile FILENAME in batch mode, ie. a clean sub-process."
  (interactive "fFile to byte-compile in batch mode: ")
  (let ((emacs (car command-line-args)))
    (compile
     (concat
      emacs " "
      (mapconcat
       'shell-quote-argument
       (list "-Q" "-batch" "-f" "batch-byte-compile" filename)
       " ")))))


;; ----------------------------------------------------------------------------
;; Enable desired features for all lisp modes
;; ----------------------------------------------------------------------------
(require-package 'rainbow-delimiters)
(require-package 'redshank)
(after-load 'redshank
  (diminish 'redshank-mode))
(after-load 'eldoc
  (diminish 'eldoc-mode))
(after-load 'aggressive-indent
  (diminish 'aggressive-indent-mode))

(defun sanityinc/enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defvar sanityinc/lispy-modes-hook
  '(rainbow-delimiters-mode
    enable-paredit-mode
    turn-on-eldoc-mode
    redshank-mode
    sanityinc/enable-check-parens-on-save)
  "Hook run in all Lisp modes.")


(when (maybe-require-package 'aggressive-indent)
  (add-to-list 'sanityinc/lispy-modes-hook 'aggressive-indent-mode))

(when (maybe-require-package 'adjust-parens)
  (defun sanityinc/adjust-parens-setup ()
    (when (fboundp 'lisp-indent-adjust-parens)
      (set (make-local-variable 'adjust-parens-fallback-dedent-function) 'ignore)
      (set (make-local-variable 'adjust-parens-fallback-indent-function) 'ignore)
      (local-set-key (kbd "<M-left>") 'lisp-dedent-adjust-parens)
      (local-set-key (kbd "<M-right>") 'lisp-indent-adjust-parens)))

  (add-to-list 'sanityinc/lispy-modes-hook 'sanityinc/adjust-parens-setup))

(defun sanityinc/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'sanityinc/lispy-modes-hook))

(defconst sanityinc/elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst sanityinc/lispy-modes
  (append sanityinc/elispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")

(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/lispy-modes))
  (add-hook hook 'sanityinc/lisp-setup))

(if (boundp 'eval-expression-minibuffer-setup-hook)
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (require-package 'eldoc-eval)
  (require 'eldoc-eval)
  (eldoc-in-minibuffer-mode 1))

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))

(require-package 'cl-lib-highlight)
(after-load 'lisp-mode
  (cl-lib-highlight-initialize))

(provide 'init-lisp)
