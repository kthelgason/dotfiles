;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(setq goto-address-mail-face 'link)

(setq-default regex-tool-backend 'perl)

(setq auto-save-default nil)

;; No friggin' bell
(setq ring-bell-function 'ignore)

;; default to utf8
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(provide 'init-misc)
