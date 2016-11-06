(in-package :cl-bodge.resources)


(declaim (special *objects*
                  *resource-path*
                  *resolvers*))


(defgeneric parse-chunk (chunk-type parameters data)
  (:method (chunk-type parameters data)
    (error "Unknown chunk type: ~a" chunk-type)))


(defun push-object (id obj)
  (setf (gethash id *objects*) obj))


(defun merge-relative (relative-path)
  (fad:merge-pathnames-as-file *resource-path* relative-path))


(defun %map-value (val mapper)
  (if (listp (first val))
      (mapcar mapper val)
      (funcall mapper val)))


(defun %push-resolver (accessor cur-id ref-ids)
  (flet ((%get-object (ref-id)
           (if-let ((ref (gethash ref-id *objects*)))
             ref
             (warn "Cannot resolve reference: object '~a' undefined" ref-id))))
    (prog1 nil
      (push (lambda ()
              (let ((refs (remove-if #'null (mapcar #'%get-object (ensure-list ref-ids)))))
                (funcall accessor refs (gethash cur-id *objects*))))
            *resolvers*))))


(defmacro define-chunk-structure ((name &optional treep child-type) &body slots)
  (with-gensyms (obj)
    (let ((ctor-name (symbolicate '%make- name))
          (slot-names (loop for slot in slots collecting
                           (if (listp slot)
                               (first slot)
                               slot)))
          (required-slots (if treep
                              '(id children)
                              '(id))))
      (flet ((%ensure-list (arg-list)
               `(let ((args ,(if treep
                                 `(first ,arg-list)
                                 `,arg-list)))
                  (ensure-list args)))
             (redefine-slot (slot id)
               (destructuring-bind (slot-name type) slot
                 `(,slot-name
                   ,(if (eq :reference type)
                        `(%push-resolver #'(setf ,(symbolicate name '- slot-name))
                                         ,id
                                         ,slot-name)
                        `(%map-value ,slot-name #',(symbolicate 'make- type)))))))
        `(progn
           (defstruct (,name
                        (:constructor ,ctor-name (,@required-slots ,@slot-names)))
             ,@required-slots
             ,@slot-names)

           (defun ,(symbolicate 'make- name) (arg-list)
             (destructuring-bind (id &key ,@slot-names) ,(%ensure-list 'arg-list)
               (with-hash-entries ((,obj id)) *objects*
                 (unless (null ,obj)
                   (warn "Redefining object '~a'" id))
                 (let ,(loop for slot in slots
                          when (listp slot) collecting
                            (redefine-slot slot 'id))
                   (setf ,obj
                         ,(if treep
                              `(,ctor-name id ,(if (null child-type)
                                                   `(rest arg-list)
                                                   `(mapcar #',(symbolicate 'make- child-type)
                                                            (rest arg-list)))
                                           ,@slot-names)
                              `(,ctor-name id ,@slot-names))))))))))))


(defclass resource ()
  ((chunks :initarg :chunks)))


(defun chunks-by-type (resource chunk-type)
  (with-slots (chunks) resource
    (gethash chunk-type chunks)))


(defun load-resource (path)
  (flet ((resolve-references (resolvers)
           (dolist (fn resolvers)
             (funcall fn))))
    (with-open-file (in (fad:canonical-pathname path))
      (destructuring-bind (format version) (read in)
        (unless (eq :brf format)
          (error "Unknown format: ~a" format))
        (unless (eql 1 version)
          (error "Unsupported version: ~a" version))
        (let* ((*objects* (make-hash-table :test 'equal))
               (*resource-path* path)
               (*resolvers* '())
               (chunk-table (make-hash-table)))
          (loop for chunk-header = (read in nil nil)
             until (null chunk-header) do
               (destructuring-bind (chunk-type &rest parameters) chunk-header
                 (with-hash-entries ((chunks chunk-type)) chunk-table
                   (push (parse-chunk chunk-type parameters (read in)) chunks))))
          (resolve-references *resolvers*)
          (make-instance 'resource :chunks chunk-table))))))
