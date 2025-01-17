(in-package #:markcl)

(defmethod markcl::apply-tag (out (tag (eql :thead)) body)
  (format out "~%")
  (dolist (form body) (markcl::render-form out form))
  (dolist (_ (cdar body)) (format out "| :---: "))
  (format out "|~%"))

(defmethod markcl::apply-tag (out (tag (eql :code-block)) body)
  (multiple-value-bind (attrs children) (markcl::extract-attrs body)
    (format out "```~a~%" (or (alexandria:assoc-value attrs :lang) ""))
    (markcl::render-forms out children)
    (format out "~%```~%")))

(defmethod markcl::apply-tag (out (tag (eql :blockquote)) body)
  (let ((lines (ppcre:split "\\n" (markcl::render-forms nil body))))
    (format out "~{> ~a~%~}~%" lines)))

(in-package #:hiccl)

(defmethod hiccl::apply-tag (out (tag (eql :br)) body)
  (declare (ignore body))
  (format out "<br>"))
