
(defparameter config-path "~/.config/lisp-notebook/")

(defun generate-id (length)
  "生成指定长度的数字编号"
  (random (ash 2 (1- (* 4 length)))))

(defun save-db (data-base filename)
  (let* ((path-to-file (concatenate 'string config-path filename)))
    (with-open-file (out path-to-file
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (with-standard-io-syntax
        (print data-base out)))))
(defmacro load-db (data-base filename)
  `(let* ((path-to-file ,(concatenate 'string config-path filename))
          (file-exists (probe-file path-to-file)))
     (when file-exists
       (with-open-file (in path-to-file
                           :if-does-not-exist :error)
         (with-standard-io-syntax
           (setf ,data-base (read in)))))))

(defun cat-text (target-name obj-name &optional (obj-folder config-path))
  (let* ((path-to-target (concatenate 'string config-path target-name))
         (path-to-obj (concatenate 'string obj-folder obj-name)))
    (shell (concatenate 'string "[ -f " path-to-target
                        " ] && cat " path-to-target " >> " path-to-obj))))
(defun append-to-text (target-string obj-name &optional (obj-folder config-path))
  (let* ((path-to-obj (concatenate 'string obj-folder obj-name)))
    (shell (concatenate 'string "echo \"" target-string "\" >> " path-to-obj))))

