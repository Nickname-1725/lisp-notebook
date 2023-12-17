(load "src/file-io.lisp")
(load "src/data-structure.lisp")

;;;; 用户交互

(load "src/features.lisp")
(load "src/user-interact.lisp")

(defun load-all ()
  (load-id id-table)
  (load-contents contents-table))

(defun init-fun ()
  (load-all) (main-repl))
