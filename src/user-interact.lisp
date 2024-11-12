(defun user-read ()
  "通用解析用户输入函数(同时判断输入的合法性)~@
  去除所有越界索引, 以及除数字,字符串,符号外的输入" ; 这里开始允许符号, 错误处理有风险
  (let* ((cmd (read-from-string
               (concatenate 'string "(" (read-line) ")" )))
         (operands (mapcar (lambda (item)
                             (cond ((numberp item)
                                    (if (or (> item (length
                                                     (cadr (list-items user-stack))))
                                            (< item 1))
                                        nil
                                        item))
                                   ((or (stringp item)
                                        (symbolp item))
                                    item)))
                           (cdr cmd))))
    (if (member nil operands) nil
        (cons (car cmd) operands))))

(defmacro user-eval* (allow-cmds &body fun-list)
  "模板，生成user-eval类型的函数，输入参数为允许的命令列表及允许词数
  allow-cmds: 应形如((command-1 3) (command-2 1))"
  `(lambda (sexp)
     (format t "~c[2J~c[H" #\escape #\escape)
     (let* ((operator-name (car sexp))
            (operands (cdr sexp))
            (allow-cmds ,allow-cmds)
            (find-cmd (assoc operator-name allow-cmds)))
       (if (and find-cmd
                (eq (length sexp) (cadr find-cmd)))
           (let ((operator (case operator-name
                             ,@fun-list)))
             (apply operator operands) t) ; 匹配到功能就返回t
           ;(format t "Not a valid command. (✿ ◕ __ ◕ )~%")
           nil)))) ; 匹配不到功能就返回nil

(defparameter user-cmd-eval
  (user-eval* '(;; 增
                (box 2) (paper 2)
                ;; 删
                (destruct 2) (trash 2)
                ;; 改
                (nvim 2) (code 2) (rename 3) (pose 3) (push-into 3) (pop-out 2)
                ;; 查
                (enter 2) (upper 1)
                ;; 保存/退出
                (dump-md-plain 2) (dump-md-styled 2) (export 2) (save 1))
    ;; 增
    (box #'(lambda (name) (new-datum user-stack 'containers name)))
    (paper #'(lambda (name) (new-datum user-stack 'sheets name)))
    ;; 删
    (destruct #'(lambda (index)
                  (let* ((item (get-item user-stack index))
                         (id (car item))
                         (type (get-type id-table id)))
                    (destruct user-stack item)
                    (if (eq 'sheets type)
                        (ignore-errors
                          (delete-file (concatenate 'string config-path
                                                    (format nil "~8,'0x" id)
                                                    ".md")))))))
    (trash #'(lambda (index)
               (let* ((item (get-item user-stack index))
                      (below (flatten contents-table item 0)))
                 (mapcar
                  (lambda (item)
                    (let* ((id (car item))
                           (type (get-type id-table id)))
                      (if (eq 'sheets type)
                          (ignore-errors
                            (delete-file (concatenate 'string config-path
                                                      (format nil "~8,'0x" id)
                                                      ".md"))))))
                  below)
                 (trash user-stack item))))
    ;; 改
    (nvim #'(lambda (index) (editor-call index "nvim")))
    (code #'(lambda (index) (editor-call index "code")))
    (rename #'(lambda (index name)
                (rename-node id-table (car (get-item user-stack index)) name)))
    (pose #'(lambda (index destine) (pose* user-stack index destine)))
    (push-into #'(lambda (index destn)
                   (if (eq 'sheets (get-type id-table
                                             (car (get-item user-stack destn))))
                       (format t "Do not push anything into sheets!~%")
                       (push-into user-stack index destn))))
    (pop-out #'(lambda (index)
                 (unless (pop-out user-stack index)
                   (format t "This has been the *root*, stop popping out anything!~%"))))
    ;; 查
    (enter #'(lambda (index)
               (if (eq 'sheets (get-type id-table (car (get-item user-stack index))))
                   (format t "You can not *enter* a sheet!~%")
                   (enter user-stack index))))
    (upper #'(lambda ()
               (unless (upper user-stack) ; 若向上目录为空, 则提示已经位于根结点
                 (format t "This has been the *root*!~%"))))
    ;; 保存/退出
    (dump-md-plain #'(lambda (index)
                       (export-markdown #'dump-plain-Markdown index)))
    (dump-md-styled #'(lambda (index)
                        (export-markdown #'dump-styled-Markdown index)))
    (save #'(lambda ()
              (save-id id-table)
              (save-contents contents-table)))))

(defun user-cmd-description (cmd-desc)
  "依次打印命令的描述"
  (format t "~{~{- [~a~19t]: ~a~}~%~}" cmd-desc))

(defun display-list ()
  "依次打印当前目录内的项目"
  (let* ((user-list (cadr(list-items user-stack)))
         (index-list (loop for i from 1 to (length user-list)
                           collect i))
         (imformation-list
           (mapcar
            (lambda (item index)
              (if (eq 'containers (get-type id-table (car item)))
                  (list index
                        (format nil "~c[1m~6A~c[0m"
                                #\escape (get-type-for-user user-stack item) #\escape)
                        (if (empty-p user-stack item)
                            (format nil "~c[31mEMPTY~c[0m" #\escape #\escape) "FULL ")
                        (count-sheets user-stack item)
                        (count-items user-stack item)
                        (get-name id-table (car item)))
                  (list index "SHEET " "-    " "-" "-" (get-name id-table (car item)))))
            user-list
            index-list)))
    (format t "======== NOW: ~c[4m~a~c[0m ========~%"
            #\escape
            (let* ((current (list-items user-stack))
                   (current-id (car current))
                   (name (get-name id-table current-id))
                   (parent-name
                     (if (eq nil name)
                         (format nil "~c[1m*ROOT*~c[1m" #\escape #\escape) name)))
              parent-name)
            #\escape)
    (format t "~{~{~a~3t~a ~a (~2,' d sheets in ~2,' d~titems) \"~a\"~}~%~}"
            imformation-list)))

(defparameter *descriptions*
  '((main-repl
     ("help add" "To add some papers and boxes.")
     ("help delete" "To delete redundant papers or boxes.")
     ("help change" "To make some changes(without adding or deleting).")
     ("help shift" "To wander around your masterpieces.")
     ("help files" "Informations about the files and QUIT."))
    (add
     ;; 增
     ("box \"name\"" "Create a container. Name shall be \"quote-marked\".")
     ("paper \"name\"" "Create a sheet. Name shall be \"quote-marked\"."))
    (delete
     ;; 删
     ("destruct indx" "Remove a container/sheet, sub-nodes will be upgraded automatically.")
     ("trash indx" "Erase a container/sheet, sub-nodes will be elimated, too."))
    (change
     ;; 改
     ("nvim index" "Edit the sheet with Noevim(shall has been installed).")
     ("code index" "Edit the sheet with VSCode(shall has been installed).")
     ("rename indx \"name\"" "Rename a container/sheet. ")
     ("pose indx destn" "Move a container/sheet to the destination.")
     ("push-into indx destn" "Push a container/sheet into the destination.")
     ("pop-out indx" "Move a container/sheet to its upper level."))
    (shift
     ;; 查
     ("enter indx" "Enter a container.")
     ("upper" "Back to upper level."))
    (files
     ;; 保存/退出
     ("dump-md-plain indx" "Export your work in plain Markdown format.")
     ("dump-md-styled indx" "Export your work in styled Markdown format.")
     ("save" "Save your masterpiece with your own hands.")
     ("quit" "Exit. Data will be automatically restored by your little helper.(˵ ✿ ◕ ‿ ◕ ˵)"))))

(defun print-description (&optional (prompt 'main-repl))
  "反馈可用命令"
  (let* ((doc (cdr (assoc prompt *descriptions*))))
    (if (eq nil doc) (print-description)
        (user-cmd-description doc))))

(defun greet-line (&optional (prompt 'main-repl))
  (case prompt
    (main-repl "The note-book (v1.4.1) launched. Wellcome back. ( ✿ ◕ ‿ ◕ )~%")
    (add "Here you go! Docs about adding some data.~%")
    (delete "Here you go! Docs about deleting data.~%")
    (change "Here you go! Docs about making some changes.~%")
    (shift "Here you go! Docs about shifting around.~%")
    (files "Here you go! Docs about files and how to quit.~%")
    (exception "Not a valid command. (✿ ◕ __ ◕ )~%")))

(defun main-repl (&optional (prompt 'main-repl))
  (format t (greet-line prompt))
  (print-description prompt)
  (display-list)
  ;; 执行用户命令
  (let ((cmd (user-read)))
    (cond ((eq (car cmd) 'quit) ; 退出笔记
           (save-id id-table)
           (save-contents contents-table)
           (format t "Exited the note-book. Goodbye."))
          ((funcall user-cmd-eval cmd) ; 一般用户功能
           (main-repl))
          ((eq (car cmd) 'help) ; 查看命令
           (let ((prompt (cadr cmd)))
             (main-repl prompt))) ; 可用命令
          (t (main-repl 'exception))))) ; 命令无效
