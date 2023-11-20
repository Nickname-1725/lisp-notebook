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
(defmethod book-list ((table contents-table))
  "获取book层级的序号列表"
  (mapcar (lambda (item) (car item)) (children-list table (tree table))))

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
(defmethod get-tree-from-node ((table contents-table) node depth id)
  "查找位于深度depth的子树node下的一个序号为id的结点"
  (macrolet ((make-pred (id) `(lambda (node) (eq ,id (car node))))
             (make-manage-hit () `(lambda (node depth) depth node)))
    (get-*-from-node table node depth (make-pred id) (make-manage-hit))))

(defmethod get-*-from-root ((table contents-table) pred manage-hit)
  "通用查找函数, 起点为根节点, 可衍生get-tree*, get-parent-tree, get-depth*"
  (let ((root (tree table)))
    (get-*-from-node table root 0 pred manage-hit)))
(defmethod get-tree ((table contents-table) id)
  "获取指定id结点"
  (macrolet ((make-pred (id) `(lambda (node) (eq ,id (car node))))
             (make-manage-hit () `(lambda (node depth) depth node)))
    (get-*-from-root table (make-pred id) (make-manage-hit))))
(defmethod get-parent ((table contents-table) id)
  "获取指定id结点的父亲"
  (if (zerop id) nil
      (macrolet
          ((make-pred (id) `(lambda (node) (assoc ,id (children-list table node))))
           (make-manage-hit () `(lambda (node depth) depth node)))
        (get-*-from-root table (make-pred id) (make-manage-hit)))))
(defmethod get-depth ((table contents-table) id)
  "计算制定id结点的深度"
  (if (zerop id) nil
      (macrolet
          ((make-pred (id) `(lambda (node) (assoc ,id (children-list table node))))
           (make-manage-hit () `(lambda (node depth) node (1+ depth))))
        (get-*-from-root table (make-pred id) (make-manage-hit)))))

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
(defmethod remove-tree ((table contents-table) node)
  "给定一个节点, 将其从contents-table中移除"
  (let* ((node-id (car node))
         (parent (get-parent table node-id)))
    (macrolet ((get-siblings () `(cadr parent)))
      (setf (get-siblings) (remove-if (lambda (x) (eq x node)) (get-siblings))))))
(defmethod save-contents ((table contents-table))
  (save-db (tree table) "contents.db"))
(defmethod load-contents ((table contents-table))
  (load-db (tree table) "contents.db"))

;;;; user-table
(defclass user-table ()
  ((disp-list :accessor get-list
              :initform '()
              :writer write-list)
   (parent :accessor get-parnt
           :initform 0
           :writer write-parnt))
  (:documentation
   "用户表, 获取contents-table的数据, 操作contents-table和id-tabel. "))

(defparameter user-table (make-instance 'user-table))

;;; 查
(defmethod get-id ((table user-table) index)
  "根据索引从`disp-list`中获取序号"
  (let ((disp-list (get-list table)))
    (if (or (< index 1) (> index (length disp-list)))
        (error "The index invalid! (shall be 1, 2, 3, ...)"))
    (elt (get-list table) (1- index))))
(defmethod empty-p ((table user-table) id)
  "根据序号判断其内部是否为空"
  (not (children-list contents-table (get-tree contents-table id))))
(defmethod get-type-for-user ((table user-table) id)
  "根据序号获取类型(containers/sheets)以及深度, 
   从而判断用户使用的类型(BOOK, CHAPTR, SECTN, SUBSEC, SS-SEC, SHEET)"
  (let ((depth (get-depth contents-table id))
        (type (get-type id-table id)))
    (if (eq type 'containers)
        (cond ((eq depth 1) 'BOOK)
              ((eq depth 2) 'CHAPTR)
              ((eq depth 3) 'SECTN)
              ((eq depth 4) 'SUBSEC)
              ((eq depth 5) 'SS-SEC))
        'SHEET)))
(defmethod count-sheets ((table user-table) id)
  "根据id递归式获取其下方sheets类型数据的个数"
  (let* ((target (get-tree contents-table id)))
    (labels ((遍历 (node)
               (cond
                 ((eq 'sheets (get-type id-table (car node))) 1)
                 (t (reduce
                     (lambda (x y)
                       (+ x (遍历 y)))
                     (children-list contents-table node) :initial-value 0)))))
      (遍历 target))))
(defmethod count-items ((table user-table) id)
  "根据id获取其下方节点个数"
  (let* ((target (get-tree contents-table id))
         (children (children-list contents-table target)))
    (length children)))

;;; 改
(defmethod update-list ((table user-table))
  "更新user-table中的`disp-list`"
  (write-list
   (mapcar #'car (children-list
                  contents-table
                  (get-tree contents-table (get-parnt table))))
   table))
(defmethod cd* ((table user-table) index)
  "获取索引对应的id, 将其设置为parent, 类似于进入文件夹"
  (write-parnt (get-id table index) table)
  (update-list table))
(defmethod cd.. ((table user-table))
  "从parent获取父节点序号, 设置为parent, 类似与切换到上一级"
  (write-parnt (car (get-parent contents-table (get-parnt table)))
               table)
  (update-list table))

(defmethod pose* ((table user-table) index destine)
  (pose contents-table
        (get-tree contents-table (get-parnt table))
        index destine)
  (update-list table))

(defmethod push-into ((table user-table) index destine)
  (let ((target (get-tree contents-table (get-id user-table index))))
    (remove-tree contents-table target)
    (contain contents-table
             target
             (get-tree contents-table (get-id user-table destine)))) 
  (update-list table))

(defmethod pop-out ((table user-table) index)
  (let ((target (get-tree contents-table (get-id user-table index))))
    (remove-tree contents-table target)
    (contain contents-table
             target
             (get-parent contents-table (get-parnt table)))) 
  (update-list table))

;;; 增
(defmethod new-datum ((table user-table) class name)
  "新建名称为`name`的数据, 类型为class(containers/sheets)"
  (let* ((new-id (make-id id-table class name))
         (parent-id (get-parnt table)))
    (contain contents-table (create-node contents-table new-id)
             (get-tree contents-table parent-id))
    (update-list table)))

;;; 删
(defmethod trash ((table user-table) index)
  "递归式清除索引为index及其下方的所有数据"
  (labels ((遍历 (id)
             (cond
               ((empty-p user-table id))
               (t (reduce
                   (lambda (x y)
                     `,x
                     (遍历 (car y)))
                   (children-list contents-table (get-tree contents-table id))
                   :initial-value nil)))
             (remove-tree contents-table (get-tree contents-table id))
             (remove-node id-table id)))
    (遍历 (get-id user-table index)))
  (update-list table))

(defmethod destruct ((table user-table) index)
  "清除索引为index的节点, 保留其子节点, 并且子节点自动上移"
  (let* ((target-id (get-id user-table index))
         (parent (get-parent contents-table target-id))
         (target (get-tree contents-table target-id))
         (children (children-list contents-table target)))
    (nconc (children-list contents-table parent) children)
    (remove-tree contents-table target)
    (remove-node id-table target-id))
  (update-list table))


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

;;;; 用户交互

(defun user-read ()
  "通用解析用户输入函数(同时判断输入的合法性)
  去除所有越界索引, 以及除数字及字符串外的输入"
  (let* ((cmd (read-from-string
               (concatenate 'string "(" (read-line) ")" )))
         (operands (mapcar (lambda (item)
                             (cond ((numberp item)
                                    (if (or (> item (length (get-list user-table)))
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
                (nvim 2)(rename 3) (pose 3) (push-into 3) (pop-out 2)
                ;; 查
                (enter 2) (upper 1)
                ;; 保存/退出
                (export 2) (save 1))
    ;; 增
    (box #'(lambda (name) (new-datum user-table 'containers name)))
    (paper #'(lambda (name) (new-datum user-table 'sheets name)))
    ;; 删
    (destruct #'(lambda (index)
                  (let* ((target-id (get-id user-table index))
                         (type (get-type id-table target-id)))
                    (destruct user-table index)
                    (if (eq 'sheets type )
                        (shell (concatenate 'string "rm -f " config-path
                                            (format nil "~8,'0x" target-id)
                                            ".md"))))))
    (trash #'(lambda (index)
               (let* ((target-id (get-id user-table index))
                      (type (get-type id-table target-id)))
                 (if (and (not (eq 0 (count-sheets user-table index)))
                          (eq type 'containers))
                     (progn
                       (format t "~c[2J~c[H" #\escape #\escape)
                       (format t "Containers remaining sheets not allowed to trash!~%")
                       (read-line))
                     (progn
                       (trash user-table index)
                       (if (eq 'sheets type )
                           (shell (concatenate 'string "rm -f " config-path
                                               (format nil "~8,'0x" target-id)
                                               ".md"))))))))
    ;; 改
    (nvim #'(lambda (index)
              (let ((target-id (get-id user-table index)))
                (if (eq 'containers (get-type id-table target-id))
                  (format t "The operation is not allowed on containers!~%")
                  (shell (concatenate 'string "nvim " config-path
                                      (format nil "~8,'0x" target-id)
                                      ".md"))))))
    (rename #'(lambda (index name)
                (rename-node id-table (get-id user-table index) name)))
    (pose #'(lambda (index destine) (pose* user-table index destine)))
    (push-into #'(lambda (index destine)
                   (if (eq 'sheets (get-type id-table (get-id user-table destine)))
                       (format t "Do not push anything into sheets!~%")
                       (push-into user-table index destine))))
    (pop-out #'(lambda (index) (pop-out user-table index)))
    ;; 查
    (enter #'(lambda (index)
               (if (eq 'sheets (get-type id-table (get-id user-table index)))
                   (format t "You can not *enter* a sheet!~%")
                   (cd* user-table index))))
    (upper #'(lambda ()
               (if (eq 0 (get-parnt user-table))
                   (format t "This has been the *root*!~%")
                   (cd.. user-table))))
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
  (let* ((user-list (get-list user-table))
         (index-list (loop for i from 1 to (length user-list)
                           collect i))
         (imformation-list
           (mapcar
            (lambda (x index)
              (if (eq 'containers (get-type id-table x))
                  (list index
                        (get-type-for-user user-table x)
                        (if (empty-p user-table x) 'empty 'full)
                        (count-sheets user-table x)
                        (count-items user-table x)
                        (get-name id-table x))
                  (list index 'sheet "-" "-" "-" (get-name id-table x))))
            user-list
            index-list)))
    (format t "======== NOW: ~a ========~%"
            (let* ((parent-id (get-parnt user-table))
                   (parent-name (if (eq 0 parent-id) '*ROOT*
                                    (get-name id-table parent-id))))
              parent-name))
    (format t "~{~{~a~3t~a~10t~a~16t(~a sheets in ~a items) ~40t\"~a\"~}~%~}"
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
     ("nvim index" "Edit the sheet with Noevim(shall have been installed).")
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
  (load-contents contents-table)
  (update-list user-table))

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
