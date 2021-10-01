(in-package :jingoh.org)

(declaim (optimize speed))

(defun the-nil-subject-procedure (org var body return)
  ;; iterate all requirements
  (let ((s (gensym "S")) (sub? (cadr var)))
    `(locally
      ;; Due to not simple-array.
      (declare (optimize (speed 1)))
      (loop :for ,s :across (! (org-specifications ,org))
                 ,@(when sub?
                     `(:for ,sub? = (spec-subject ,s)))
            :do (map nil (lambda (,(car var)) ,@body) (spec-requirements ,s))
            :finally (return
                      (let (,@var)
                        (declare (ignorable ,@var))
                        ,return))))))

(defun the-subject-procedure (var body gname org return)
  (alexandria:with-unique-names (specifications subject)
    `(let ((,specifications (! (org-specifications ,org))))
       (dolist
           (,(or (cadr var) subject) (alexandria:ensure-list ,gname)
                                     (let (,(car var))
                                       (declare (ignorable ,(car var)))
                                       ,return))
         (map nil (lambda (,(car var)) ,@body)
              (spec-requirements
                (?!
                 (find ,(or (cadr var) subject) ,specifications
                       :key #'spec-subject))))))))

(defmacro do-requirements
          (&whole whole
           (var &optional (subject-designator t) (org '*org*) return-form)
           &body body)
  (check-bnf:check-bnf (:whole whole)
    ((var (or symbol subject-spec))
     (subject-spec (symbol symbol)))
    ((subject-designator symbol))
    ((org check-bnf:expression))
    ((return-form check-bnf:expression)))
  (setf var (alexandria:ensure-list var))
  (let ((gname (gensym "NAME")))
    `(macrolet ((?! (form)
                  `(or ,form
                       (error 'missing-subject
                              :api 'do-requirements
                              :datum ,',gname)))
                (! (form)
                  `(resignal-bind ((type-error () 'not-org
                                     :api 'do-requirements))
                     ,form)))
       (let ((,gname ,subject-designator))
         (case ,gname
           ((nil) ,(the-nil-subject-procedure org var body return-form))
           ((t)
            ,(the-subject-procedure var body
                                    `(setf ,gname (org-current-subjects *org*))
                                    org return-form))
           (otherwise
            ,(the-subject-procedure var body gname org return-form)))))))

(define-compiler-macro do-requirements
                       (&whole whole
                        (var
                         &optional
                         (subject-designator t)
                         (org '*org*)
                         return)
                        &body body)
  (setf var (alexandria:ensure-list var))
  (if (not (constantp subject-designator))
      whole
      `(macrolet ((?! (form)
                    `(or ,form
                         (error 'missing-subject
                                :api 'do-requirements
                                :datum ,',subject-designator)))
                  (! (form)
                    `(resignal-bind ((type-error () 'not-org
                                       :api 'do-requirements))
                       ,form)))
         ,(case subject-designator
            ((nil) (the-nil-subject-procedure org var body return))
            ((t)
             (the-subject-procedure var body '(org-current-subjects *org*) org
                                    return))
            (otherwise
             (the-subject-procedure var body subject-designator org return))))))

(macrolet ((! (n form)
             `(resignal-bind ((error () 'not-org
                                :datum org
                                :expected-type 'org
                                :api
                                ',(nth n '(map-requirements add-requirement))))
                ,form))
           (?! (form)
             `(or ,form
                  (error 'missing-subject :api 'map-requirements :datum sub))))
  (defun map-requirements (function &optional (subject t) (org *org*))
    (declare (optimize (speed 1))) ; due to not simple-array.
    (flet ((s-reqs (sub)
             (spec-requirements
               (?! (find sub (! 0 (org-specifications org))
                         :key #'spec-subject)))))
      (case subject
        ((nil) ; all subject.
         (loop :for spec :across (! 0 (org-specifications org))
               :nconc (map 'list function (spec-requirements spec))))
        ((t) ; current subject
         (loop :for sub :in (org-current-subjects *org*)
               :nconc (map 'list function (s-reqs sub))))
        (otherwise (map 'list function (s-reqs subject))))))
  (defun add-requirement (subject requirement &optional (org *org*))
    (check-type subject symbol)
    (let ((spec
           (find subject (! 1 (org-specifications org)) :key #'spec-subject)))
      (if spec
          (vector-push-extend requirement (spec-requirements spec))
          (vector-push-extend (spec subject requirement)
                              (org-specifications org))))
    requirement)) ; end of macrolet

(defun find-subject (subject &optional (org *org*))
  (declare (optimize (speed 1))) ; due to not simple-array.
  (loop :for s :across (org-specifications org)
        :when (eq subject (spec-subject s))
          :return s))