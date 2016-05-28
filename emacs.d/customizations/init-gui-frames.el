
;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(setq indicate-empty-lines t)
(setq ns-use-srgb-colorspace nil)

(set-fringe-style '(8 . 0))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(when (fboundp 'toggle-frame-fullscreen)
  ;; Ctrl-Option-f to toggle fullscreen mode
  (global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (unless window-system
                (set-frame-parameter nil 'menu-bar-lines 0)))))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

(provide 'init-gui-frames)
