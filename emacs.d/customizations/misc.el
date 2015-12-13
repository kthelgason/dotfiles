;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

(setq inhibit-startup-message   t
      use-dialog-box nil
      gc-cons-threshold 20000000
      ns-pop-up-frames nil
      search-highlight           t
      query-replace-highlight    t
      read-file-name-completion-ignore-case t
      next-line-add-newlines t
      read-buffer-completion-ignore-case t
      completion-auto-help 'lazy
      isearch-allow-scroll t
      visible-bell nil
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'post-forward
      uniquify-ignore-buffers-re "^\\*")

