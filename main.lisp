 (defun generate-id (length)
  "生成指定长度的数字编号"
  (random (ash 2 (1- (* 4 length)))))

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
  "创建一个指定类型及名称的数据; 
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
       (write-shts new-list table)))))

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

;;;; 目录表及其方法
(defclass contents-table ()
  ((tree :accessor tree
         :initform '(0)
         :writer write-tree))
  (:documentation
   "contents-table, 储存id的树状层次关系; 
   具有增加, 删除, 调整顺序, 查找功能"))
(defparameter contents-table (make-instance 'contents-table))

(defmethod insert ((table contents-table) id target)
  "给定一个序号, 将其插入到target下方"
  (macrolet ((sub-target (target) `(cadr ,target)))
    (cond
      ((eq nil (sub-target target)) (nconc target (list (list (list id)))))
      (t (push (list id) (sub-target target))))))
(defmethod get-tree ((table contents-table) id)
  "给定一个序号, 查找其在目录表中的数状结构"
  (let* ((root (tree table)))
    (labels
        ((遍历 (id node)
           (macrolet ((children-list (node) `(cadr ,node)))
             (let ((current (car node)))
               (if
                (eq id current) node
                (reduce (lambda (x y)
                          (cond (x x)
                                (t (遍历 id y))))
                        (children-list node) :initial-value nil))))))
      (遍历 id root))))
