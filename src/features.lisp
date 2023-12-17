
(defun editor-call (index editor)
  "用给定editor打开指定纸张对应的文件, editor须为字符串"
  (let ((target-id (car (get-item user-stack index))))
    (if (eq 'containers (get-type id-table target-id))
        (format t "The operation is not allowed on containers!~%")
        (shell (concatenate 'string editor " " config-path
                            (format nil "~8,'0x" target-id)
                            ".md")))))

(defun dump-plain-Markdown (struct-list)
  "去掉struct头部, 获得toc-list; ~@
  遍历整个toc-list, 对于每个元素都先查看是否为sheets, ~@
  然后再决定追加字符串还是拼接文本"
  (let* ((toc-list (cdr struct-list)))
    (reduce
     (lambda (x item) `,x
       (let* ((id (car item))
              (type (get-type id-table id)))
         (cond
           ((eq 'containers type) ; 容器: 生成标题
            (let ((caption-name
                    (concatenate
                     'string (case (cadr item)
                               (1 "# ") (2 "## ") (3 "### ") (4 "#### "))
                     (get-name id-table id))))
              (append-to-text caption-name "preview.md")))
           ((eq 'sheets type) ; 纸张: 拼接文本
            (let ((target-file-name
                    (concatenate 'string (format nil "~8,'0x" id) ".md")))
              (cat-text target-file-name "preview.md"))))))
     toc-list :initial-value nil)))

(defun make-html-tag (tagname class id)
  `(,(format nil "<~a~a~a>" tagname
             (if (eq nil class) "" (format nil " class=\\\"~a\\\"" class))
             (if (eq nil id) "" (format nil " id=\\\"~a\\\"" id)))
    ,(format nil "</~a>" tagname)))

(defun dump-styled-toc (toc-list)
  (let ((div-tag-toc (make-html-tag "div" "tableofcontents" nil)))
    (append-to-text (car div-tag-toc) "preview.md")
    (append-to-text "" "preview.md") ; 必须空一行
    (reduce
     (lambda (x item) `,x
       (let* ((id (car item))
              (type (get-type id-table id)))
         (if (eq 'containers type) ; 容器, 生成目录
             (append-to-text
              (concatenate 'string (case (cadr item)
                                     (1 "- ") (2 "  - ") (3 "    - "))
                           (format nil "[~a](#~a)"
                                   (get-name id-table id)
                                   (format nil "~8,'0x" id)))
              "preview.md"))))
     toc-list :initial-value nil)
    (append-to-text (cadr div-tag-toc) "preview.md")
    (append-to-text "" "preview.md"))) ; 必须空一行
(defun dump-styled-Markdown (struct-list)
  "去掉struct头部, 获得toc-list; ~@
  获取struct头部, 获得title; ~@
  遍历整个toc-list, 对于每个元素都先查看是否为sheets, ~@
  然后再决定追加字符串还是拼接文本"
  (let* ((toc-list (cdr struct-list))
         (title (get-name id-table (car (car struct-list))))
         (title-tag (make-html-tag "title" nil nil))
         (div-tag-main (make-html-tag "div" "main" nil)))
    (append-to-text (concatenate 'string (car title-tag) title (cadr title-tag))
                    "preview.md")
    (append-to-text (format nil "<link rel=\\\"stylesheet\\\" href=\\\"style.css\\\"/>")
                    "preview.md") ; 引入CSS
    (dump-styled-toc toc-list)
    (append-to-text (car div-tag-main) "preview.md")
    (reduce
     (lambda (x item) `,x
       (let* ((id (car item))
              (type (get-type id-table id)))
         (cond
           ((eq 'containers type) ; 容器: 生成标题
            (let* ((heading-tag
                     (make-html-tag (format nil "h~a" (cadr item))
                                    nil
                                    (format nil "~8,'0x" id)))
                   (caption-name
                     (concatenate
                      'string (car heading-tag)
                      (get-name id-table id) (cadr heading-tag))))
              (append-to-text caption-name "preview.md")))
           ((eq 'sheets type) ; 纸张: 拼接文本
            (let ((div-tag-outer (make-html-tag "div" "sheet-wrap" nil))
                  (div-tag-caption (make-html-tag "div" "sheet-caption" nil))
                  (target-file-name
                    (concatenate 'string (format nil "~8,'0x" id) ".md")))
              (append-to-text
               (concatenate 'string (car div-tag-outer)
                            (car div-tag-caption) (get-name id-table id)
                            (cadr div-tag-caption))
               "preview.md")
              (append-to-text "" "preview.md")
              (cat-text target-file-name "preview.md")
              (append-to-text (cadr div-tag-outer) "preview.md"))))))
     toc-list :initial-value nil)
    (append-to-text (cadr div-tag-main) "preview.md")))

(defun export-markdown (dump-fun index)
  "调用dump-fun(dump-plain-Markdown/dump-styled-Markdown)"
  (let* ((ustack (access user-stack))
         (book (if (eq nil ustack)
                   (let* ((item (get-item user-stack index))
                          (id (car item))
                          (type (get-type id-table id)))
                     (if (eq 'sheets type)
                         (format t "Don't export a single sheet!~%")
                         item))
                   (car (last ustack)))))
    (funcall dump-fun (flatten contents-table book 0))))

