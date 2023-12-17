(load "src/file-io.lisp")
(load "src/data-structure.lisp")

;;;; 用户交互

(load "src/features.lisp")
(load "src/user-interact.lisp")

(defun load-all ()
  (load-id id-table)
  (load-contents contents-table))

(defun main-repl ()
  (format t "The note-book launched. Wellcome back. ( ✿ ◕ ‿ ◕ )~%")
  (print-description)
  (display-list)
  ;; 执行用户命令
  (let ((cmd (user-read)))
    (if (eq (car cmd) 'quit)
        (progn
          "退出笔记"
          (save-id id-table)
          (save-contents contents-table)
          (format t "Exited the note-book. Goodbye."))
        (progn
          (funcall user-cmd-eval cmd)
          (main-repl)))))

(defun init-fun ()
  (load-all) (main-repl))
