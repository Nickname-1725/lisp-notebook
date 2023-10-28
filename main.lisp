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

;;; 数据结构的节点生成
(defmethod create-node ((table contents-table) id)
  "给定一个序号, 生成一个节点"
  (list id))

;;; 查找操作
(defmethod get-* ((table contents-table) id pred)
  "get-tree, get-parent, get-depth的通用抽象, 主要为遍历部分"
  (let* ((root (tree table)))
    (labels
        ((遍历 (id node depth)
           (macrolet ((children-list (node) `(cadr ,node)))
             (if (funcall pred node id) (list node depth)
                 (reduce
                  (lambda (x y)
                    (cond (x x)
                          (t (遍历 id y (1+ depth)))))
                  (children-list node) :initial-value nil)))))
      (遍历 id root 1))))
(defmethod get-tree ((table contents-table) id)
  "给定一个序号, 查找其在目录表中的数状结构"
  (car (get-* contents-table id (lambda (node id) (eq id (car node))))))
(defmethod get-parent ((table contents-table) id)
  "给定一个序号, 查找其在目录表中的父节点"
  (if (zerop id) nil
      (car (get-* contents-table id (lambda (node id) (assoc id (cadr node)))))))
(defmethod get-depth ((table contents-table) id)
  "给定一个序号, 查找其位于目录表树状图中的层次; 其中, root深度为0"
  (if (zerop id) 0
      (cadr (get-* contents-table id (lambda (node id) (assoc id (cadr node)))))))

;;; 树操作
(defmethod contain ((table contents-table) node container)
  "给定一个节点node, 将其插入到container下方"
  (macrolet ((cont-inner () `(cadr container)))
    (cond
      ((eq nil (cont-inner)) (nconc container (list (list node))))
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

;;; 联合封装的操作
(defmethod insert ((table contents-table) id target-id)
  "给定一个序号, 生成节点并将其插入到序号为target-id的节点下方"
  (contain table (create-node table id) (get-tree table target-id)))



