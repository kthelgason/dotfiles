(require 'tuareg)
(require 'utop)

(autoload 'utop-minor-mode "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)

(setq opam-share
      (substring (shell-command-to-string "opam config var share") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

(setq ocamlbuild-command
  (concat "ocamlbuild -use-ocamlfind"
          " -pkgs sexplib.syntax,sexplib,core"
          " -tags thread,principal"
          " -syntax camlp4o "))

(defun corebuild (buffer)
  (interactive (list (buffer-name)))
  (compile (concat ocamlbuild-command
                   (substring buffer 0 -3)
                   ".native")))

(define-key tuareg-mode-map (kbd "C-c C-c") 'corebuild)

(provide 'setup-ocaml)

