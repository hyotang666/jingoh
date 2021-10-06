(in-package :jingoh.documentizer)

(declaim (optimize speed))

(define-condition missing-spec-file (style-warning)
  ((path :initarg :path :reader path))
  (:report
   (lambda (condition stream)
     (format stream "Missing spec file. ~S" (path condition)))))

(defun missing-spec-file (pathname) (warn 'missing-spec-file :path pathname))

(defstruct (meta-data (:constructor %make-meta-data)
                      (:copier nil)
                      (:predicate nil))
  (name nil :type symbol) ; pacakge-name
  (exports nil :type list) ; exported symbol extract from defpackage form.
  (doc nil :type (or null string)) ; documentation extract from defpacakge form.
  (sections nil :type list) ; all sections
  (singles nil :type list) ; sections which specified requirement.
  (commons nil :type list) ; sections which specified common requirement.
  (specifieds nil :type list) ; symbols which specified.
  )

(defun make-meta-data (form)
  (let* ((pathname
          (make-pathname :name (string-downcase
                                 (locally ; due to generics.
                                  (declare (optimize (speed 1)))
                                  (string (second form))))
                         :type "lisp"
                         :defaults *default-pathname-defaults*))
         (sections (parse-spec pathname)))
    (multiple-value-bind (singles commons)
        (sieve sections)
      (%make-meta-data :name (second form)
                       :exports (loop :for option :in (cddr form)
                                      :when (eq :export (car option))
                                        :append (cdr option))
                       :doc (let ((option (assoc :documentation (cddr form))))
                              (when option
                                (second option)))
                       :sections sections
                       :singles singles
                       :commons commons
                       :specifieds (apply #'append
                                          (loop :for sec :in sections
                                                :collect (section-names
                                                           sec)))))))

(defun sieve (meta-data-sections)
  (loop :for sec :in meta-data-sections
        :if (single-p sec)
          :collect sec :into singles
        :if (common-p sec)
          :collect sec :into commons
        :finally (return (values singles commons))))

(declaim
 (ftype (function (asdf:system &optional pathname)
         (values list ; of-type meta-data
                 &optional))
        meta-datas<=system))

(defun meta-datas<=system
       (system &optional (sys-dir (asdf:system-source-directory system)))
  (let ((*default-pathname-defaults*
         (let ((spec-dir (merge-pathnames "spec/" sys-dir)))
           (or (uiop:probe-file* spec-dir)
               (return-from meta-datas<=system (missing-spec-file spec-dir))))))
    (mapcar #'make-meta-data (defpackage-forms<-system system))))

(defun defpackage-forms<-system (system)
  (unless (asdf:component-loaded-p system)
    (asdf:load-system system))
  (let ((package *package*))
    (unwind-protect
        (uiop:while-collecting (acc)
          (dolist (component (component-children system))
            (with-open-file (s (asdf:component-pathname component))
              (do* ((tag '#:tag)
                    (hook (coerce *macroexpand-hook* 'function))
                    (*macroexpand-hook* (lambda (expander form env)
                                          (typecase form
                                            ((cons (eql in-package) *)
                                             (eval
                                               (funcall hook expander form
                                                        env)))
                                            ((not (cons (eql defpackage) *))
                                             (funcall hook expander form env))
                                            (otherwise ; defpackage form.
                                             (acc form)
                                             (funcall hook expander form
                                                      env)))))
                    (exp #0=(read s nil tag) #0#))
                   ((eq exp tag))
                (macroexpand exp)))))
      (setq *package* package))))

(defun component-children (component)
  (if (typep component 'asdf:package-inferred-system)
      (warn "Currently package-inferred-system is not supported.")
      (labels ((rec (list acc)
                 (if (endp list)
                     acc
                     (body (car list) (cdr list) acc)))
               (body (first rest acc)
                 (typecase first
                   (asdf:system
                    (rec (append (asdf:component-children first) rest) acc))
                   (asdf:module
                    (rec (append (asdf:component-children first) rest) acc))
                   (asdf:static-file (rec rest acc))
                   (otherwise (rec rest (cons first acc))))))
        (rec (list component) nil))))