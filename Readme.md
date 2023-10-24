# 目的
1. 一个层级化笔记管理工具
2. 使用nvim作为编辑器，可以编写Markdown笔记单元(sheet)
3. 每个笔记单元都以类似文件的方式放置在层级化结构中
4. 层级化结构为：chapter, section, subsection, ss-section (或可自定义)
5. 允许移动文件夹/笔记单元
   - `succ target destination`: 将目标项目`target`移动到指定项目`destination`后方
   - `pred target destination`: 将目标项目`target`移动到指定项目`destination`前方
6. 允许新建文件夹/笔记单元
7. 允许编辑文件夹/笔记单元的名称，允许编辑笔记单元的内容
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

     或者
     ```lisp
     (defun generate-id (length)
     "生成指定长度的字符串编号"
       (format nil (concatenate 'string "~" 
                     (princ-to-string length) ",'0x") 
         (random (ash 2 (1- (* 4 length))))))
     (generate-id 8)
     ```

     或者
     ```lisp
     (defun generate-id (length)
     "生成指定长度的数字编号"
       (random (ash 2 (1- (* 4 length)))))
     (generate-id 8)
     ```
8. 允许查看文件夹/笔记单元的名称，允许查看笔记单元的内容
   - 以目录层级的形式呈现，一次只呈现一个文件夹内的内容
   - 在列表左侧标号
   - 向用户提示命令
   - 以nvim的形式查看
9. 允许导出
   - 允许将笔记本以文件目录/.db的形式导出
   - 允许将笔记本以markdown的形式导出
   - 暂时不考虑markdown的渲染问题，markdown的渲染由导出后的VS Code实现
10. 以 **随机不重复序** 号的形式存储文件夹/笔记单元的信息，从而允许重名

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
2. 方法
   1. `generate-id ()`: 生成随机化8位序号
   2. `make-id (class name)`: 构造一个特定名字和类型的数据
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
   1. `insert (id target)`: 给定一个序号, 将其插入到`target`的下方
   2. `get-tree (id)`: 给定一个序号，查找其在目录表中的数状结构
   3. `get-parent (id)`: 给定一个序号，查找目录表中其父节点
   4. `get-depth (id)`: 给定一个序号, 查找其位于数状图的层次;\
      0: root\
      1: book\
      2: chapter\
      3: section\
      4: sub-section\
      5: sub-sub-section
   5. `destruct (id)`: 给定一个序号，从目录表中删除它，目录表中的层级自动上移
   6. `push-into (target destine)`: 将给定target压入destine内部层级
   7. `pop-out (target)`: 将给定target弹出，置于上一层级
   8. `succ (target destine)`: 将给定target置于destine前方
   9. `pred (target destine)`: 将给定target置于destine后方
#### 用户表
1. 数据结构
   ```lisp
   (id-7 id-8)
   ```



