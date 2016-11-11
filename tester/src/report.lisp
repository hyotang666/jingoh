(in-package :jingoh.tester)

(defstruct issue form expected actual test position)

(defstruct(condition-issue (:include issue))
  message)

(defstruct(error-was-signaled (:include condition-issue)))

(defstruct(warning-was-signaled (:include condition-issue)))

(defstruct(debugger-was-invoked (:include condition-issue)))

(defstruct(unmatch-condition (:include condition-issue)))

(defstruct(unexpected-success (:include issue)))

(defstruct(unexpected-output (:include issue)))

(defstruct(issue-of-multiple-values (:include issue)))

(defstruct(wrong-format (:include issue)))

(defstruct(missing-restarts(:include issue)))

(defstruct(unsatisfied-clause (:include issue))
  args)

#+(or sbcl)
(defmethod make-instance :around ((type structure-class) &rest args)
  (loop :with instance = (closer-mop:class-prototype type)
	:for slot :in (closer-mop:class-slots type)
	:for name = (closer-mop:slot-definition-name slot)
	:do(setf (slot-value instance name)
		 (or (getf args (intern(symbol-name name):keyword))
		     (funcall(closer-mop:slot-definition-initfunction slot))))
	:finally (return instance)))

