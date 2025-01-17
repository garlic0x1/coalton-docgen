(in-package #:coalton-docgen)

(defvar *format* :markdown
  "Set either `:markdown' or `:html' to choose output format.")

(defvar *url-root* "/"
  "This is used for generating GitHub links.")

(defmacro render (out &body forms)
  "Format-agnostic renderer."
  `(case *format*
     (:html     (hiccl:render  ,out ,@forms))
     (:markdown (markcl:render ,out ,@forms))))

(defun sanitize (string)
  "Format-agnostic sanitizer, not secure."
  (case *format*
    (:html (progn
             (setf string (format nil "~a" string))
             string))
    (:markdown (progn
                 (setf string (format nil "~a" string))
                 (setf string (ppcre:regex-replace-all "\\*" string "\\*"))
                 (ppcre:regex-replace-all "`" string "\\\\`")))))

(defun function-doc (sym)
  "Get a docstring for a Coalton function without the type."
  (second
   (ppcre:split "\\n"
                (documentation sym 'function)
                :limit 2)) )

(defun exportedp (sym)
  "Check if a symbol is exported from its package."
  (eq :external (nth-value 1 (find-symbol (symbol-name sym) (symbol-package sym)))))

(defun package-classes (package)
  "List of exported classes in `package'."
  (let ((classes '()))
    (fset:do-map (sym entry (algo:immutable-map-data
                             (tc:environment-class-environment
                              entry:*global-environment*)))
      (when (eql (find-package package) (symbol-package sym))
        (when (exportedp sym)
          (push (cons sym entry) classes))))
    classes))

(defun package-instances (package)
  "List of instances implemented in `package'. (UNUSED)"
  (let ((instances '()))
    (fset:do-map (sym entry (algo:immutable-map-data
                             (tc:environment-class-environment
                              entry:*global-environment*)))
      (when (eql (find-package package) (symbol-package sym))
        (push (cons sym entry) instances)))
    instances))

(defun package-methods (package)
  "List of exported methods in `package'."
  (remove-if-not
   #'exportedp
   (remove-duplicates
    (mapcar
     (lambda (method) (cons (env:ty-class-method-name method) method))
     (a:mappend
      (lambda (pair) (tc:ty-class-unqualified-methods (cdr pair)))
      (package-classes package)))
    :key #'car)
   :key #'car))

(defun package-types (package)
  "List of exported types in `package'."
  (let ((types '()))
    (fset:do-map (sym entry (algo:immutable-map-data
                             (tc:environment-type-environment
                              entry:*global-environment*)))
      (when (eql (find-package package) (symbol-package sym))
        (when (exportedp sym)
          (push (cons sym entry) types))))
    types))

(defun package-values (package)
  "List of exported values in `package',
minus ones that are methods or types."
  (let ((types (package-types package))
        (methods (package-methods package))
        (values '()))
    (fset:do-map (sym entry (algo:immutable-map-data
                             (tc:environment-value-environment
                              entry:*global-environment*)))
      (when (eql (find-package package) (symbol-package sym))
        (when (exportedp sym)
          (unless (or (find sym types :key #'car)
                      (find sym methods :key #'car))
            (push (cons sym entry) values)))))
    values))

(defun format-type (sym entry)
  `(:<>
    (:h3 ,(format nil "~a :: " (sanitize sym))
         (:i ,(sanitize (tc:kind-of entry))))
    (:blockquote ,(source:docstring entry))))

(defun format-type-db (package)
  (a:when-let ((types (package-types package)))
    `(:<>
      (:h2 "Types")
      ,@(mapcar (lambda (pair) (format-type (car pair) (cdr pair)))
                types))))

(defun format-class (sym entry)
  `(:<>
    (:h3
     ,(let ((class-pred (tc:ty-class-predicate entry)))
        `(:<> ,(format nil "~a :: " sym)
              (:i ,(tc:with-pprint-variable-context ()
                     (format nil "[~a :: ~a]"
                             (tc:ty-predicate-types class-pred)
                             (sanitize
                              (mapcar #'tc:kind-of
                                      (tc:ty-predicate-types class-pred)
                                      ))))))))
    (:blockquote ,(source:docstring entry))
    (:ul
     ,@(mapcar
        (lambda (method)
          `(:li (:h4 ,(format nil "~a :: " (tc:ty-class-method-name method))
                     (:i ,(format nil "~a" (coalton:type-of (tc:ty-class-method-name method)))))
                ,(a:when-let ((doc (function-doc (tc:ty-class-method-name method))))
                   `(:blockquote ,(sanitize doc)))))
        (tc:ty-class-unqualified-methods entry)))))

(defun format-class-db (package)
  (a:when-let ((classes (package-classes package)))
    `(:<>
      (:h2 "Classes")
      ,@(mapcar (lambda (pair) (format-class (car pair) (cdr pair)))
                classes))))

(defun format-value (sym entry)
  `(:<>
    (:h3 ,(format nil "~a :: " (sanitize sym))
         (:i ,(sanitize entry)))
    (:blockquote ,(sanitize (function-doc sym)))))

(defun format-value-db (package)
  (a:when-let ((values (package-values package)))
    `(:<>
      (:h2 "Values")
      ,@(mapcar (lambda (pair) (format-value (car pair) (cdr pair)))
                values))))

(defun format-docs (package)
  "Format documentation for a package into renderable s-expressions."
  `(:<>
    (:h1 ,package)
    ,@(remove-if-not
       #'identity
       (list
        (format-type-db package)
        (format-class-db package)
        (format-value-db package)))))

(defun render-docs (out package)
  "Render documentation for `package' to `out' stream, in the format
specified by `coalton-docgen:*format*'."
  (render out (format-docs package)))
