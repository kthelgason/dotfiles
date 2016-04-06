(require-package 'unfill)

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))

(electric-indent-mode 1)

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(transient-mark-mode t)
(blink-cursor-mode 0)

(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "s-<return>") 'sanityinc/newline-at-end-of-line)

(after-load 'subword (diminish 'subword-mode))

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(show-paren-mode 1)

(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require-package 'highlight-escape-sequences)
(hes-mode)

(setq-default show-trailing-whitespace t)

;;; Whitespace

(defun sanityinc/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'sanityinc/no-trailing-whitespace))


(require-package 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)
(diminish 'whitespace-cleanup-mode)

(provide 'init-editing)


