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
10. 以哈希值的形式存储文件夹/笔记单元的信息，从而允许重名


