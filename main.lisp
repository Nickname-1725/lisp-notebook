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

;;;; id-table及其方法
(defclass id-table ()
  ((containers :accessor cont
               :initform '()
               :writer write-cont)
   (sheets :accessor shts
           :initform '()
           :writer write-shts))
  (:documentation "id-table, 将容器和纸张匿名化"))

(defparameter id-table (make-instance 'id-table))

(defmethod make-id ((table id-table) type name)
  "创建一个指定类型及名称的数据;返回id 
  1. type: 'containers / 'sheets
  2. name: 一个字符串"
  (let* ((id (generate-id 8))
         (id-list-cont (cont table))
         (id-list-shts (shts table))
         (id-list (append id-list-cont id-list-shts))
         (id-pair (list id name)))
    (cond
      ; 若id已被占用, 则重新尝试
      ((assoc id id-list) (make-id table type name))
      ; 将id-pair插入列表中
      ((eq type 'containers)
       (write-cont (cons id-pair id-list-cont) table) id)
      ((eq type 'sheets)
       (write-shts (cons id-pair id-list-shts) table) id))))

(defmethod get-node ((table id-table) id method)
  "根据id-表中的id获取结点; 优先获取containers, 然后是sheets"
  (let* ((container-list (cont table))
         (sheet-list (shts table))
         (container-found (assoc id container-list))
         (sheet-found (assoc id sheet-list))
         (found (cond (container-found `(containers ,container-found))
                      (sheet-found `(sheets ,sheet-found)))))
    (funcall method found)))
(defmethod get-name ((table id-table) id)
  "根据id-表中的id查找名称"
  (get-node table id (lambda (found) (cadadr found))))
(defmethod get-type ((table id-table) id)
  "根据id-表中的id查找类型"
  (get-node table id (lambda (found) (car found))))
(defmethod rename-node ((table id-table) id name)
  "给定id, 重命名该数据"
  (let* ((type (get-type table id))
         (new-node (list id name)))
    (cond ((eq type 'containers)
           (setf (cont table)
                 (cons new-node
                       (remove (assoc id (cont table)) (cont table)))))
          ((eq type 'sheets)
           (setf (shts table)
                 (cons new-node
                       (remove (assoc id (shts table)) (shts table))))))))
(defmethod remove-node ((table id-table) id)
  "给定id, 重命名该数据"
  (let* ((type (get-type table id)))
    (cond ((eq type 'containers)
           (setf (cont table)
                 (remove (assoc id (cont table)) (cont table))))
          ((eq type 'sheets)
           (setf (shts table)
                 (remove (assoc id (shts table)) (shts table)))))))
(defmethod save-id ((table id-table))
  (save-db (shts table) "sheets-id.db")
  (save-db (cont table) "containers.db"))
(defmethod load-id ((table id-table))
  (load-db (shts table) "sheets-id.db")
  (load-db (cont table) "containers.db"))

;;;; 目录表及其方法
(defclass contents-table ()
  ((tree :accessor tree
         :initform '(0 nil)
         :writer write-tree))
  (:documentation
   "contents-table, 储存id的树状层次关系; 
   具有增加, 删除, 调整顺序, 查找功能"))
(defparameter contents-table (make-instance 'contents-table))

;;; 数据结构的节点生成
(defmethod create-node ((table contents-table) id)
  "给定一个序号, 生成一个节点"
  (list id nil))

;;; 查找操作
(defmethod children-list ((table contents-table) node)
  (cadr node))

(defmethod get-*-from-node ((table contents-table) node depth pred manage-hit)
  "带短路的遍历通用查找函数 ~@
  depth是node所处的深度; pred是是否命中查找的谓词; manage-hit处理返回何值 ~@
  由于是带短路, 所以一旦找到, reduced-children不作处理即返回"
  (labels ((遍历 (node depth)
             "遍历; ~@
             reduce不会处理空列表"
             (if (funcall pred node)
                 (funcall manage-hit node depth)
                 (let ((reduced-children
                         (reduce
                          (lambda (x child)
                            (cond (x x)
                                  (t (遍历 child (1+ depth)))))
                          (children-list table node) :initial-value nil)))
                   reduced-children))))
    (遍历 node depth)))
(defmethod flatten ((table contents-table) node depth)
  "目录中给定一个节点, 遍历其下方所有节点, 返回扁平化结构"
  (labels ((遍历 (node depth)
             "遍历; depth是node的所在深度; ~@
             原本以为需要判断叶子结点, 实际上不需要, 因为reduce不会处理空列表"
             (let ((flatten-children
                     (reduce
                      (lambda (x child)
                        (append x (遍历 child (1+ depth))))
                      (children-list table node) :initial-value nil)))
               (append (list (list (car node) depth)) flatten-children))))
    (遍历 node depth)))

;;; 树操作
(defmethod contain ((table contents-table) node container)
  "给定一个节点node, 将其插入到container下方"
  (macrolet ((cont-inner () `(cadr container)))
    (cond
      ;((eq nil (cont-inner)) (nconc container (list (list node))))
      (t (setf (cont-inner) (cons node (cont-inner)))))))
(defmethod pose ((table contents-table) node index destine)
  "给定索引, 将node下索引为index的元素插入到索引为destine的位置; 这里的索引从1开始"
  (macrolet ((get-lst () `(cadr node)))
    ;; 卫语句, 判断索引越界
    (if (or (< index 1) (> index (length (get-lst)))
            (< destine 1) (> destine (length (get-lst))))
        (error "Invalid indexes"))
    ;; 根据destine与index的相对位置, 确定分割线split-index, 然后重新拼接列表
    (flet ((filt-redundent (lst elem)
             (remove-if (lambda (x) (eq x elem)) lst)))
      (let* ((lst (get-lst))
             (split-index (if (< index destine) destine (1- destine)))
             (elem (nth (1- index) lst))
             (before (filt-redundent (subseq lst 0 split-index) elem))
             (after  (filt-redundent (subseq lst split-index) elem)))
        (setf (get-lst) (append before (list elem) after))))))
(defmethod remove-from-parent ((table contents-table) parent node)
  "给定node的父结点, 从子结点中移除node"
  (macrolet ((get-siblings () `(cadr parent)))
    (setf (get-siblings) (remove-if (lambda (x) (eq x node)) (get-siblings)))))
(defmethod save-contents ((table contents-table))
  (save-db (tree table) "contents.db"))
(defmethod load-contents ((table contents-table))
  (load-db (tree table) "contents.db"))

;;;; 用户栈及其方法
(defclass user-stack ()
  ((stack :accessor access
          :initform '()))
  (:documentation "用户栈，储存进入树结构中，当前结点和向上的结点，除了根节点"))
(defparameter user-stack (make-instance 'user-stack))

(defmethod list-items ((ustack user-stack))
  "从用户栈中读取当前的列表(目录树结构的结点)"
  (let* ((current-list (car (access ustack))))
    (if (eq nil current-list)
        (tree contents-table) ; 如果栈空, 则访问根节点
        current-list)))
(defmethod list-siblings ((ustack user-stack))
  (let* ((senior-parent (cadr (access ustack))))
    (if (eq nil senior-parent)
        (tree contents-table) ; 如果栈空, 则访问根节点
        senior-parent)))
;;; 查
(defmethod get-item ((ustack user-stack) index)
  "根据索引从当前列表中获取项目"
  (nth (1- index) (cadr (list-items ustack))))
(defmethod empty-p ((ustack user-stack) item)
  "判断指定项目内部是否为空"
  (not (children-list contents-table item)))
(defmethod count-depth ((ustack user-stack))
  "获取当前所在目录树的深度"
  (length (access ustack)))
(defmethod get-type-for-user ((ustack user-stack) item)
  "获取指定项目的类型(containers/sheets)以及深度,~@
  从而判断用户使用的类型(BOOK, CHAPTR, SECTN, SUBSEC, SS-SEC, SHEET)"
  (let* ((id (car item))
         (depth (1+ (count-depth ustack)))
         (type (get-type id-table id)))
    (if (eq type 'containers)
        (case depth (1 'BOOK) (2 'CHAPTR) (3 'SECTN) (4 'SUBSEC) (5 'SS-SEC))
        'SHEET)))
(defmethod count-sheets ((ustack user-stack) item)
  (let* ((below (flatten contents-table item 0))
         (sheet-list (remove-if-not
                      (lambda (item)
                        "获得所有纸张"
                        (let* ((id (car item))
                               (type (get-type id-table id)))
                          (eq type 'sheets)))
                      below)))
    (length sheet-list)))
(defmethod count-items ((ustack user-stack) item)
  (let* ((children (children-list contents-table item)))
    (length children)))

;;; 改
(defmethod enter ((ustack user-stack) index)
  "获取索引对应的项目, 进入该项目, 并且压入栈内"
  (push (get-item ustack index) (access ustack)))
(defmethod upper ((ustack user-stack))
  "将当前项目弹出栈, 类似于切换到上一级(自动防止根节点操作, 若操作, 返回nil)"
  (pop (access ustack)))
(defmethod pose* ((ustack user-stack) index destn)
  "将当前目录下index移动到索引为destn的位置"
  (pose contents-table (list-items ustack) index destn))
(defmethod push-into ((ustack user-stack) index destn)
  (let* ((target (get-item ustack index))
         (current (list-items ustack))
         (destn-item (get-item ustack destn)))
    (remove-from-parent contents-table current target)
    (contain contents-table target destn-item)))
(defmethod pop-out ((ustack user-stack) index)
  (let* ((target (get-item ustack index))
         (current (list-items ustack))
         (senior-parent (list-siblings ustack)))
    (remove-from-parent contents-table current target)
    (contain contents-table target senior-parent)
    (upper ustack)))

;;; 增
(defmethod new-datum ((ustack user-stack) class name)
  "新建名称为name的数据, 类型为class(containers/sheets)"
  (contain contents-table
           (create-node contents-table (make-id id-table class name))
           (list-items ustack)))

;;; 删
(defmethod trash ((ustack user-stack) item)
  "清除item的结点及其下方的所有数据(id单独清除)"
  (let* ((below (flatten contents-table item 0))
         (current (list-items ustack)))
    (remove-from-parent contents-table current item) ; 可优化
    (mapcar (lambda (item)
              (let ((id (car item)))
                (remove-node id-table id)))
            below)))
(defmethod destruct ((ustack user-stack) item)
  "清除item的结点, 保留其子结点, 并且子结点自动上移"
  (let* ((current (list-items ustack))
         (item item)
         (id (car item))
         (children (children-list contents-table item)))
    (nconc (children-list contents-table current) children)
    (remove-from-parent contents-table current item)
    (remove-node id-table id)))

;;;; 用户交互


(defun editor-call (index editor)
  "用给定editor打开指定纸张对应的文件, editor须为字符串"
  (let ((target-id (car (get-item user-stack index))))
    (if (eq 'containers (get-type id-table target-id))
        (format t "The operation is not allowed on containers!~%")
        (shell (concatenate 'string editor " " config-path
                            (format nil "~8,'0x" target-id)
                            ".md")))))

(defun user-read ()
  "通用解析用户输入函数(同时判断输入的合法性)
  去除所有越界索引, 以及除数字及字符串外的输入"
  (let* ((cmd (read-from-string
               (concatenate 'string "(" (read-line) ")" )))
         (operands (mapcar (lambda (item)
                             (cond ((numberp item)
                                    (if (or (> item (length
                                                     (cadr (list-items user-stack))))
                                            (< item 1))
                                        nil
                                        item))
                                   ((stringp item) item)))
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
             (apply operator operands))
           (format t "Not a valid command. (✿ ◕ __ ◕ )~%")))))

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
                (export 2) (save 1))
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
                        (shell (concatenate 'string "rm -f " config-path
                                            (format nil "~8,'0x" id)
                                            ".md"))))))
    (trash #'(lambda (index)
               (let* ((item (get-item user-stack index))
                      (below (flatten contents-table item 0)))
                 (trash user-stack item)
                 (mapcar
                  (lambda (item)
                    (let* ((id (car item))
                           (type (get-type id-table id)))
                      (if (eq 'sheets type)
                          (format t "~a" (concatenate 'string "rm -f " config-path
                                              (format nil "~8,'0x" id)
                                              ".md"))
                          (shell (concatenate 'string "rm -f " config-path
                                              (format nil "~8,'0x" id)
                                              ".md")))))
                  below))))
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
    (export #'(lambda (indx)
                (format t "~c[2J~c[H" #\escape #\escape)
                (format t "Sorry about that. The feature is not ready yet! That's awkward.(┭┮ ﹏ ┭┮ )~%")
                (read-line)
                `,indx
                '(do-something-here)))
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

(defun print-description ()
  "反馈可用命令"
  (user-cmd-description
   '(;; 增
     ("box \"name\"" "Create a container. Name shall be \"quote-marked\".")
     ("paper \"name\"" "Create a sheet. Name shall be \"quote-marked\".")
     ;; 删
     ("destruct indx" "Remove a container/sheet, sub-nodes will be upgraded automatically.")
     ("trash indx" "Erase a container/sheet, sub-nodes will be elimated, too.")
     ;; 改
     ("nvim index" "Edit the sheet with Noevim(shall has been installed).")
     ("code index" "Edit the sheet with VSCode(shall has been installed).")
     ("rename indx \"name\"" "Rename a container/sheet. ")
     ("pose indx destn" "Move a container/sheet to the destination.")
     ("push-into indx destn" "Push a container/sheet into the destination.")
     ("pop-out indx" "Move a container/sheet to its upper level.")
     ;; 查
     ("enter indx" "Enter a container.")
     ("upper" "Back to upper level.")
     ;; 保存/退出
     ("export indx" "Export your work in Markdown format.")
     ("save" "Save your masterpiece with your own hands.")
     ("quit" "Exit. Data will be automatically restored by your little helper.(˵ ✿ ◕ ‿ ◕ ˵)"))))

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
