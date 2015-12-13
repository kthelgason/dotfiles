;;; package --- keybindings
;;; Commentary:

;;; Code:
(require 'evil)
(require 'paredit)

(define-key evil-insert-state-map "\C-e" nil)
(define-key evil-insert-state-map "\C-y" nil)

;; use regexp search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-i") 'idomenu)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(define-key evil-visual-state-map ",x" 'smex)
(define-key evil-visual-state-map ",X" 'smex-major-mode-commands)
(define-key evil-normal-state-map ",l" 'ido-switch-buffer)
(define-key evil-normal-state-map ",x" 'smex)
(define-key evil-normal-state-map ",X" 'smex-major-mode-commands)
(define-key evil-normal-state-map ",m" 'magit-status)

;; search with space
(evil-define-key 'normal global-map (kbd "SPC") 'isearch-forward-regexp)

(define-key global-map (kbd "s-+") 'text-scale-increase)
(define-key global-map (kbd "s--") 'text-scale-decrease)

;; Comments
(define-key evil-visual-state-map ",cc" 'comment-or-uncomment-region)
(define-key evil-normal-state-map ",cc" 'evilnc-comment-or-uncomment-lines)

;; Insert mode
(define-key evil-insert-state-map (kbd "M-h") 'evil-backward-char)
(define-key evil-insert-state-map (kbd "M-j") 'evil-next-line)
(define-key evil-insert-state-map (kbd "M-k") 'evil-previous-line)
(define-key evil-insert-state-map (kbd "M-l") 'evil-forward-char)

;; Easy pane switching
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;;Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)

(evil-define-key 'normal paredit-mode-map (kbd "M-<right>") 'paredit-forward-slurp-sexp)
(evil-define-key 'normal paredit-mode-map (kbd "M-<left>") 'paredit-forward-barf-sexp)
(evil-define-key 'normal paredit-mode-map (kbd "M-<up>") 'paredit-backward-slurp-sexp)
(evil-define-key 'normal paredit-mode-map (kbd "M-<down>") 'paredit-backward-barf-sexp)

(evil-define-key 'insert global-map (kbd "RET") 'newline-and-indent)

(global-set-key "\C-w" 'evil-window-map)

(provide 'keybindings)

