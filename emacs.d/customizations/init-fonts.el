;;; Character sets

(defun sanityinc/maybe-use-default-font-for-symbols ()
  "Force Emacs to render symbols using the default font, if so configured."
  (set-fontset-font "fontset-default" 'symbol (face-attribute 'default :family)))

(add-hook 'after-init-hook 'sanityinc/maybe-use-default-font-for-symbols)

;;; Changing font sizes

(require-package 'default-text-scale)
(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)


(defun sanityinc/maybe-adjust-visual-fill-column ()
  "Readjust visual fill column when the global font size is modified.
This is helpful for writeroom-mode, in particular."
  (if visual-fill-column-mode
      (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
    (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t)))

(add-hook 'visual-fill-column-mode-hook
          'sanityinc/maybe-adjust-visual-fill-column)


(set-face-attribute 'default nil :font "Source Code Pro:h16" :height 140)

(provide 'init-fonts)
