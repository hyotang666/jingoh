(in-package :jingoh.org)

(declaim (optimize speed))

(defun <iterate-all-requirements> (<org> var body <return>)
  (let ((s (gensym "S")) (sub? (cadr var)))
    `(locally
      ;; Due to not simple-array.
      (declare (optimize (speed 1)))
      (loop :for ,s :across (org-specifications ,<org>)
                 ,@(when sub?
                     `(:for ,sub? = (spec-subject ,s)))
            :do (map nil (lambda (,(car var)) ,@body) (spec-requirements ,s))
            :finally (return
                      (let (,@var)
                        (declare (ignorable ,@var))
                        ,<return>))))))

(defun <iterate-specified-subjects-requirements>
       (var body gname <org> <return>)
  (alexandria:with-unique-names (specifications subject)
    `(let ((,specifications (org-specifications ,<org>)))
       (dolist
           (,(or (cadr var) subject) (alexandria:ensure-list ,gname)
                                     (let (,(car var))
                                       (declare (ignorable ,(car var)))
                                       ,<return>))
         (map nil (lambda (,(car var)) ,@body)
              (spec-requirements
                (or (find ,(or (cadr var) subject) ,specifications
                          :key #'spec-subject)
                    (error 'missing-subject
                           :api 'do-requirements
                           :datum ',gname))))))))

(defmacro do-requirements
          (&whole whole
           (var &optional (<subject-designator> t) (<org> '*org*) <return>)
           &body body &environment env)
  (check-bnf:check-bnf (:whole whole)
    ((var (or symbol subject-spec))
     (subject-spec (symbol symbol)))
    ((<subject-designator> check-bnf:expression))
    ((<org> check-bnf:expression))
    ((<return> check-bnf:expression)))
  #-check-bnf
  (progn
   whole ; to muffle unused style warning.
   (check-type var (or symbol (cons symbol (cons symbol null)))))
  (setf var (alexandria:ensure-list var))
  (if (constantp <subject-designator> env)
      (let ((sd (eval <subject-designator>)))
        (case sd
          ((nil) (<iterate-all-requirements> <org> var body <return>))
          ((t)
           (<iterate-specified-subjects-requirements> var body
                                                      '(org-current-subjects
                                                         *org*)
                                                      <org> <return>))
          (otherwise
           (<iterate-specified-subjects-requirements> var body
                                                      <subject-designator>
                                                      <org> <return>))))
      (let ((gname (gensym "NAME")))
        `(let ((,gname ,<subject-designator>))
           (case ,gname
             ((nil) ,(<iterate-all-requirements> <org> var body <return>))
             ((t)
              ,(<iterate-specified-subjects-requirements> var body
                                                          `(setf ,gname
                                                                   (org-current-subjects
                                                                     *org*))
                                                          <org> <return>))
             (otherwise
              ,(<iterate-specified-subjects-requirements> var body gname <org>
                                                          <return>)))))))

(declaim
 (ftype (function (symbol t &optional org) (values t &optional))
        add-requirement))

(defun add-requirement (subject requirement &optional (org *org*))
  #+(or clisp allegro abcl)
  (check-type subject symbol)
  (let ((spec (find subject (org-specifications org) :key #'spec-subject)))
    (if spec
        (vector-push-extend requirement (spec-requirements spec))
        (vector-push-extend (spec subject requirement)
                            (org-specifications org))))
  requirement)

(defun find-subject (subject &optional (org *org*))
  (declare (optimize (speed 1))) ; due to not simple-array.
  (loop :for s :across (org-specifications org)
        :when (eq subject (spec-subject s))
          :return s))