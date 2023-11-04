 (defun generate-id (length)
  "生成指定长度的数字编号"
  (random (ash 2 (1- (* 4 length)))))

(defun save-db (data-base filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (with-standard-io-syntax
      (print data-base out))))
(defmacro load-db (data-base filename)
  `(let ((file-exists (probe-file ,filename)))
     (when file-exists
         (with-open-file (in ,filename
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
         (id-list (cond ((eq type 'containers) (cont table))
                        ((eq type 'sheets) (shts table))))
         (id-pair (list id name))
         (new-list (cons id-pair id-list)))
    (cond
      ; 若id已被占用, 则重新尝试
      ((assoc id id-list) (make-id table type name))
      ; 将id-pair插入列表中
      ((eq type 'containers)
       (write-cont new-list table))
      ((eq type 'sheets)
       (write-shts new-list table)))
    id))

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
  (save-db (shts table) "./.config/sheets-id.db")
  (save-db (cont table) "./.config/containers.db"))
(defmethod load-id ((table id-table))
  (load-db (shts table) "./.config/sheets-id.db")
  (load-db (cont table) "./.config/containers.db"))

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
(defmethod get-* ((table contents-table) id pred)
  "get-tree, get-parent, get-depth的通用抽象, 主要为遍历部分"
  (let* ((root (tree table)))
    (labels
        ((遍历 (id node depth)
           (if (funcall pred node id) (list node depth)
               (reduce
                (lambda (x y)
                  (cond (x x)
                        (t (遍历 id y (1+ depth)))))
                (children-list table node) :initial-value nil))))
      (遍历 id root 1))))
(defmethod get-tree ((table contents-table) id)
  "给定一个序号, 查找其在目录表中的数状结构"
  (car (get-* contents-table id (lambda (node id) (eq id (car node))))))
(defmethod get-parent ((table contents-table) id)
  "给定一个序号, 查找其在目录表中的父节点"
  (if (zerop id) nil
      (car (get-* table id (lambda (node id)
                             (assoc id (children-list table node)))))))
(defmethod get-depth ((table contents-table) id)
  "给定一个序号, 查找其位于目录表树状图中的层次; 其中, root深度为0"
  (if (zerop id) 0
      (cadr (get-* table id (lambda (node id)
                              (assoc id (children-list table node)))))))

;;; 树操作
(defmethod contain ((table contents-table) node container)
  "给定一个节点node, 将其插入到container下方"
  (macrolet ((cont-inner () `(cadr container)))
    (cond
      ;((eq nil (cont-inner)) (nconc container (list (list node))))
      (t (push node (cont-inner))))))
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
  (save-db (tree table) "./.config/contents.db"))
(defmethod load-contents ((table contents-table))
  (load-db (tree table) "./.config/contents.db"))

;;; 联合封装的操作
(defmethod insert ((table contents-table) id target-id)
  "给定一个序号, 生成节点并将其插入到序号为target-id的节点下方"
  (contain table (create-node table id) (get-tree table target-id)))

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
(defmethod cd ((table user-table) index)
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
  (insert contents-table (make-id id-table class name) (get-parnt table))
  (update-list table))

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
  (let* ((parent (get-tree contents-table (get-parnt user-table)))
         (target (get-tree contents-table (get-id user-table index)))
         (children (children-list contents-table target)))
    (nconc (children-list contents-table parent) children)
    (remove-tree contents-table target))
  (update-list table))

;;; 测试例
(load-id id-table)
(load-contents contents-table)
(update-list user-table)

(save-id id-table)
(save-contents contents-table)

;;;; 用户交互

(defun user-read ()
  "通用解析用户输入函数"
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")" ))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar (lambda (item)
                                (if (or (numberp item)
                                        (stringp item))
                                    item
                                    (funcall #'quote-it item)))
                              (cdr cmd))))))

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
                (create 2) (nvim 2)
                ;; 删
                (destruct 2) (trash 2)
                ;; 改
                (rename 3) (pose 2) (push-into 1) (pop-out 1)
                ;; 查
                (enter 2) (upper 1)
                ;; 保存/退出
                (export 2) (save 1))
    ;; 增
    (create #'(lambda (name) (new-datum user-table 'containers name)))
    (nvim #'(lambda (name) `,name '(do-something-here)))
    ;; 删
    (destruct #'(lambda (index) (destruct user-table index)))
    ;; 改
    (rename #'(lambda (index name)
                (rename-node id-table (get-id user-table index) name)))
    (pose #'(lambda (index destine) (pose* user-table index destine)))
    (push-into #'(lambda (index destine) (push-into user-table index destine)))
    (pop-out #'(lambda (index) (pop-out user-table index)))
    ;; 查
    (enter #'(lambda (index) (cd user-table index)))
    (upper #'(lambda () (cd.. user-table)))
    ;; 保存/退出
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
    (format t "~{~{~a~3t~a~10t~a~16t(~a sheets in ~a items) ~40t\"~a\"~}~%~}"
            imformation-list)))

(defun print-description ()
  "反馈可用命令"
  (user-cmd-description
   '(;; 增
     ("create \"name\"" "Create a container. Name shall be \"quote-marked\".")
     ("nvim \"name\"" "Create a sheet or edit it. Name shall be \"quote-marked\".")
     ;; 删
     ("destruct indx" "Remove a container/sheet, sub-nodes will be upgraded automatically.")
     ("trash indx" "Erase a container/sheet, sub-nodes will be elimated, too.")
     ;; 改
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
