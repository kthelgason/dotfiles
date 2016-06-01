(require-package 'evil-leader)
(require 'evil-leader)
(evil-leader/set-leader ",")
(global-evil-leader-mode t)

;; Here's what we've all been waiting for.
;; Recreate Vim inside Emacs.
(require-package 'evil)
(require 'evil)
(evil-mode 1)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-w-in-emacs-state t)
(setq evil-search-module        'isearch)
(setq evil-magic                'very-magic)
(setq evil-emacs-state-cursor   '("#dfaf8f" box))
(setq evil-normal-state-cursor  '("#f8f893" box))
(setq evil-insert-state-cursor  '("#f8f893" bar))
(setq evil-replace-state-cursor '("#cc9393" box))
(setq evil-want-fine-undo t)
(setq evil-want-change-word-to-end t)

(evil-set-initial-state 'flycheck-error-list-mode 'normal)
(evil-set-initial-state 'git-commit-mode 'insert)
(evil-set-initial-state 'shell-mode 'emacs)
(evil-set-initial-state 'esup-mode 'emacs)
(evil-set-initial-state 'diff-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'sql-interactive-mode 'emacs)
(evil-set-initial-state 'multi-term-mode 'emacs)
(evil-set-initial-state 'cider-stacktrace-mode 'emacs)
(evil-set-initial-state 'inferior-js-mode 'emacs)
(evil-set-initial-state 'alchemist-mix-mode 'emacs)

(evil-define-text-object sigsegv/evil-next-match (count &optional beg end type)
  "Select next match."
  (evil-ex-search-previous 1)
  (evil-ex-search-next count)
  (list evil-ex-search-match-beg evil-ex-search-match-end))

(evil-define-text-object sigsegv/evil-previous-match (count &optional beg end type)
  "Select previous match."
  (evil-ex-search-next 1)
  (evil-ex-search-previous count)
  (list evil-ex-search-match-beg evil-ex-search-match-end))

(define-key minibuffer-local-map [escape] 'sigsegv/minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'sigsegv/minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'sigsegv/minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'sigsegv/minibuffer-keyboard-quit)

(defun sigsegv/delete-trailing-whitespace-at-line ()
  "Delete trailing whitespace on the current "
  (interactive)
  (let ((begin (line-beginning-position))
        (end   (line-end-position)))
    (delete-trailing-whitespace begin end)))

(defvar sigsegv/last-insertion-end 0
  "The distance between point at the time of insert and beginning of line.
This tracks the saved value of the last insertion so we can figure out whether
to indent for it.")

(defvar sigsegv/last-insertion-distance 0
  "The distance between point at the time of insert and beginning of line.
This tracks the saved value of the last insertion so we can figure out whether
to indent for it.")

(defun sigsegv/sensible-to-indent-p ()
  "Determine whether it's sensible to indent the current line automagically.
Using the data stored from sigsegv/exit-insert-state, this function determines
whether or not it makes sense to indent the following line. The point of this
is to ensure we don't indent lines after the user has manually tabbed point to
the beginning of a line, but we do indent lines if there was already an
indentation from the last insert state.
A potential future improvement is to (rather than blindly indenting according
to mode, which is a potshot) indent intelligently to the saved state of point."
  (and (> sigsegv/last-insertion-distance 0)
       (sigsegv/current-line-is-empty)))

(evil-define-motion sigsegv/append-and-indent (count)
  "Moves to end of line, enters insert mode, and also indents the line."
  (evil-append-line count)
  (when (sigsegv/sensible-to-indent-p)
    (indent-according-to-mode)))

(defun sigsegv/save-insert-state-state ()
  "Save information about the state of insert state.
This does the following:
- Sets sigsegv/last-insertion-end to the character position at the end of the last
  insert state.
- Sets sigsegv/last-insertion-line to the position at the beginning of the line from
  the last insert state.
- Sets sigsegv/last-insertion-distance to the distance between those two points.
- Deletes trailing whitespace to the left of point.
The intent of this is to save the state of the insertion environment, so we can
make smart decisions based on it later."
  (interactive)
  (setq sigsegv/last-insertion-end
        (save-excursion
          (if (not (sigsegv/current-line-is-empty))
              (beginning-of-line-text))
          (point)))
  (setq sigsegv/last-insertion-line
        (save-excursion
          (goto-char sigsegv/last-insertion-end)
          (line-beginning-position)))
  (setq sigsegv/last-insertion-distance
        (- sigsegv/last-insertion-end sigsegv/last-insertion-line)))

(evil-define-motion sigsegv/ret-and-indent (count)
  "Move the cursor COUNT lines down.
If point is on a widget or a button, click on it.
In Insert state, insert a newline and indent."
  :type line
  (sigsegv/save-insert-state-state)
  (sigsegv/delete-trailing-whitespace-at-line)
  (evil-ret-gen count nil)
  (when (sigsegv/sensible-to-indent-p)
    (indent-according-to-mode)))

(defun sigsegv/what-line ()
  "Get the line, without printing the word 'line' before it."
  (1+ (count-lines 1 (point))))


(defun sigsegv/where-beginning-of-visual-line ()
  "Calculate the difference between the beginning
of the current visual line and point."
  (interactive)
  (let ((old-point (point))
        (bovl (save-excursion (beginning-of-visual-line)
                              (point))))
    (- old-point bovl)))

(defun sigsegv/is-this-line-empty ()
  "Returns t if the current line is empty. Otherwise nil."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*$")))

(defun sigsegv/current-line-is-empty ()
  "Returns t when the current line is empty or contains only whitespace."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "^\s*$")))


(defun sigsegv/electric-append-with-indent (count &optional vcount)
  "Indent the current line if it is empty.
Otherwise, just do a normal append-line."
  (interactive "p")
  (if (and (= (point) (line-beginning-position))
           (sigsegv/is-this-line-empty))
      (indent-according-to-mode))
  (evil-append-line count vcount))

(defun sigsegv/exit-insert-state ()
  "Function to be run when Evil exits insert state."
  (sigsegv/save-insert-state-state)
  (if (sigsegv/current-line-is-empty)
      (delete-horizontal-space t)))

(defun sigsegv/enter-insert-state ()
  "Function to be run when Evil enters insert state.
Loads indent data from sigsegv/sensible-to-indent-p and uses that to determine
whether to call indent-according-to-mode."
  (interactive)
  (if (sigsegv/sensible-to-indent-p)
      (indent-according-to-mode)))

(defun sigsegv/select-line ()
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil))

(defun sigsegv/comment ()
  (interactive)
  (if (not (use-region-p))
      (sigsegv/select-line))
  (comment-or-uncomment-region (region-beginning) (region-end)))

;; exiting insert mode -> delete trailing whitespace
(add-hook 'evil-insert-state-exit-hook 'sigsegv/exit-insert-state)
(add-hook 'evil-insert-state-entry-hook 'sigsegv/enter-insert-state)

(define-key evil-normal-state-map (kbd "RET") 'sigsegv/append-and-indent)
(define-key evil-normal-state-map (kbd "C-w }") 'evil-window-rotate-downwards)
(define-key evil-normal-state-map (kbd "C-w {") 'evil-window-rotate-upwards)

(define-key evil-insert-state-map (kbd "RET") 'sigsegv/ret-and-indent)
(define-key evil-insert-state-map (kbd "<s-backspace>")
  'backward-delete-char-untabify)
(define-key evil-insert-state-map (kbd "<s-return>")
  'electric-indent-just-newline)
(define-key evil-normal-state-map (kbd "<s-return>")
  'electric-indent-just-newline)

(define-key evil-normal-state-map (kbd "C-q")   'universal-argument)
(define-key evil-normal-state-map (kbd "C-h")   'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j")   'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k")   'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l")   'evil-window-right)
(define-key evil-normal-state-map (kbd "-") (kbd "dd"))

(define-key evil-normal-state-map "a"           'evil-append)
(define-key evil-normal-state-map "q"           nil)
(define-key evil-normal-state-map "A"           'sigsegv/electric-append-with-indent)
(define-key evil-normal-state-map "$"           'sigsegv/smart-end)
(define-key evil-normal-state-map "0"           'sigsegv/smart-home)

(define-key evil-motion-state-map "h"           'evil-backward-char)
(define-key evil-motion-state-map "j"           'evil-next-visual-line)
(define-key evil-motion-state-map "k"           'evil-previous-visual-line)
(define-key evil-motion-state-map "l"           'evil-forward-char)
(define-key evil-motion-state-map "$"           'evil-end-of-line)
(define-key evil-motion-state-map "0"           'evil-beginning-of-line)

(define-key evil-normal-state-map (kbd "SPC")   'evil-search-forward)
(define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

(evil-leader/set-key
  "a" 'ag
  "cc" 'sigsegv/comment
  "h" 'help)

(evil-ex-define-cmd "Q"  'evil-quit)
(evil-ex-define-cmd "Qa" 'evil-quit-all)
(evil-ex-define-cmd "QA" 'evil-quit-all)

(provide 'init-evil)
