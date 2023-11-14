# 目的
1. 使用nvim作为编辑器，可以编写Markdown笔记单元(sheet)
2. 一个层级化笔记管理工具
   1. 每个笔记单元都以类似文件的方式放置在层级化结构中
   2. 层级化结构为：chapter, section, subsection, ss-section (或可自定义)

## 待办事项
1. [ ] 数据结构的改名
   - [ ] 将`id-table`改名为`name-dictionary`
   - [ ] 将`contents-table`改名为`toc-tree`
   - [ ] 将`user-table`改名为`user-list`或`user-stack`
2. [ ] 将三个数据结构方法的入口对象从`table`改名为`n-dict`, `t-tree`, `u-stack`
   - 从而尝试是否能实现多态(相同函数名, 对于不同对象实现不同功能). 
2. [ ] 将`id-table`(`name-dictionary`)取消成员的区分(`sheets`, `containers`), 共同存放在同一个列表中
3. [ ] 实现`user-stack`
   - 对于用户从`root`结点进入子结点, 将所有途径的结点压入栈内
   - 从某种意义上是仿照文件目录的路径表达
   - 从而取消`update-list`方法的使用
   - 甚至有可能改变许多命令的实现方式, 同时节省时间和空间

## 功能详细信息
1. [x] 以 **随机不重复序** 号的形式存储容器/纸张的信息，从而允许重名
2. [x] 基础操作
   1. [x] 允许容器/纸张移动
      - `(pose index destination)`: 将索引为`index`的目标项目移动到索引为`destination`的指定项目后方
   2. [x] 允许新建容器/纸张
   3. [x] 允许查看容器/纸张的名称，允许查看纸张的内容
      - 以目录层级的形式呈现，一次只呈现一个容器内的内容
      - 在列表左侧标
        - 序号
        - 类型(BOOK, CHAPTR, SECTN, SUBSEC, SS-SEC, SHEET)
        - 容器是否为空(EMPTY); 否则不显示
      - 向用户提示命令
      - 以nvim的形式查看
   4. [x] 允许编辑容器/纸张的名称，允许编辑纸张的内容
      - 以nvim的形式编辑
        ```lisp
        (shell (concatenate 'string "~/桌面/"
          (format nil 
            (concatenate 'string "~" 
              (princ-to-string length) ",'0x")
            id)
        ```
        以上例子仅限clisp。使用clisp是因为命令最为简单
      - 生成不重复随机编号，从而允许重名
        ```lisp
        (format nil "~8,'0x" (random (ash 2 (1- (* 4 8)))))
        ```
        生成长度为8的16进制随机字符串编号
        \
        或者
        ```lisp
        (defun generate-id (length)
        "生成指定长度的字符串编号"
          (format nil (concatenate 'string "~" 
                        (princ-to-string length) ",'0x") 
            (random (ash 2 (1- (* 4 length))))))
        (generate-id 8)
        ```
        \
        或者
        ```lisp
        (defun generate-id (length)
        "生成指定长度的数字编号"
          (random (ash 2 (1- (* 4 length)))))
        (generate-id 8)
        ```
3. [ ] 允许导出
   - [ ] 允许将笔记本以文件夹内: 目录(.db), 纸张(.md)的形式导出
   - [ ] 允许将笔记本拼接并以markdown的形式导出
   - [ ] 暂时不考虑markdown的渲染问题，markdown的渲染由导出后的VS Code实现
   - [ ] 或者Markdown的渲染通过html文件与marked来实现

## 目录层级化数据管理
### 数据结构及方法
#### id-表
1. 数据结构
  ```lisp
   (('containers
     (id-1 name-1) (id-2 name-2) (id-3 name-3) (id-4 name-4)   ; books
     (id-5 name-5) (id-6 name-6) (id-7 name-7) (id-8 name-8)   ; chapters
     (id-9 name-9) (id-10 name-10) (id-11 name-11)             ; sections
     (id-12 name-12)                                           ; sub-sections
     (id-13 name-13) (id-14 name-14))                          ; subsub-sections
    ('sheets
     (id-15 name-15)))                                         ; sheets
   ```
   或者
   ```lisp
   (defclass id-table ()
     ((containers :initform '())
      (sheets :initform '())))
   ```
2. 方法
   1. `generate-id ()`: 生成随机化8位序号
   2. `make-id (class name)`: 构造一个特定名字和类型的数据; 该函数返回id
   3. `get-name (id)`: 给定一个序号，查找其名称
   4. `get-type (id)`: 给定一个序号, 查找其属性`(container / sheet)`
   5. `rename (id)`: 给定一个序号，重命名
   6. `remove (id)`: 给定一个序号，从id表中删除它
#### 目录表
1. 数据结构
   ```lisp
   (0 ; 0代表根id
    (id-1 
      (id-5) (id-6))
    (id-2
      (id-7
        (id-9) (id-10)) 
      (id-8 
        (id-11)
          (id-12
            (id-13) (id-14))))
    (id-3
      (id-15))
    (id-4))
   ```
2. 方法
   1. `create-node (id)`: 给定一个序号, 生成一个节点
   2. `get-* (id pred)`: 查找函数的通用抽象
       1. `children-list (node)`: 给定一个节点`node`, 查找其子列表, 帮助`get-*`方法
       2. `get-tree (id)`: 给定一个序号，查找其在目录表中的数状结构
       3. `get-parent (id)`: 给定一个序号，查找目录表中其父节点
       4. `get-depth (id)`: 给定一个序号, 查找其位于树状图的层次;\
          0: root\
          1: book\
          2: chapter\
          3: section\
          4: sub-section\
          5: sub-sub-section
   3. 树操作
       1. `contain (node container)`: 给定一个节点`node`, 将其插入到`container`下方
       2. `pose (node index destine)`: 将node下给定索引为index的元素置于索引为destine的位置; 索引从1开始
       3. `remove-tree (node)`: 给定一个树状结构, 从目录表中删除它
   4. `insert (id target)`: (通过`create-node`, `get-tree`, `contain`联合封装)给定一个序号, 将其插入到`target`的下方
#### 用户表
1. 数据结构
   ```lisp
   (defclass user-table ()
     ((disp-list :initform '(id-7 id-8)
      (parent :initform id-2))))
   ```
2. 方法
   1. **增**
      1. `new-datum (class name)`: 新建名称为`name`的数据, 类型为`class`(`containers`, `sheets`); 调用`id-table`生成序号, 调用`contents-table`插入序号, 调用`update-list`更新
   2. **查**
      1. `get-id (index)`: 根据索引从`disp-list`中获取序号
      2. `empty-p (id)`: 根据序号判断是否为空, 调用`contents-table`. 
      3. `get-class (id)`: 从`id-table`中获取类型(containers, sheets); 从`contents-table`中获取位于树中的层次; 判断(BOOK, CHAPTR, SECTN, SUBSEC, SS-SEC, SHEET)
      4. `count-sheets (id)`: 从`contents-table`中递归统计下方含有的`sheets`总数
      5. `count-items (id)`: 从`contents-table`中调用`children-list`方法, 获取下方含有的子节点个数
   3. **改**
      1. `update-list ()`: 更新数据结构中的`disp-list`
      2. `cd (index)`:获取索引为`index`对应的`contents-table`结点, 调用`update-list`更新
      3. `cd.. ()`: 根据`parent`从`contents-table`中获取节点, 调用`update-list`更新
      4. `pose (index destine)`: 调用`contents-table`中的`pose (index destine)`, 调用`update-list`更新
      5. `push-into (index destine)`: 调用`contents-table`中的`contain (node container)`, 调用`update-list`更新
      6. `pop-out (index)`: 调用`contents-table`中的`contain (node container)`, 调用`update-list`更新
   4. **删**
      命名上`trash`和`distruct`需要明显区分
      1. `trash (index)`: 根据索引递归删除下方的数据, 调用`contents-table`递归删除数据, 多次调用`id-table`删除数据
      2. `distruct (index)`: 根据索引解体当前容器, 将容器下方的数据释放上来, 调用`contents-table`更改树结构, 调用`id-table`删除容器. 


