(in-package :jingoh.org)

(defstruct(org (:copier nil))
  #.(Doc :jingoh.org "doc/org.T.md")
  (name nil :type symbol :read-only t)
  (package *package* :type package :read-only t)
  (current-subjects `(nil) :type cons)
  (options nil :type list)
  (specifications (make-array 0 :fill-pointer 0 :adjustable t) :type vector))

(defmethod print-object((o org)*standard-output*)
  (if(null *print-escape*)
    (call-next-method)
    (print-unreadable-object(o *standard-output* :type t)
      (let((count(org-requirements-count o)))
	(format t "~A~:[ ~D ~:*requirement~P~;~]"
		(org-name o)
		(zerop count)
		count)))))

(deftype org-designator()
  #.(Doc :jingoh.org "doc/org-designator.T.md")
  '(or (and symbol (not boolean))
       org))

(deftype subject-designator()
  #.(Doc :jingoh.org "doc/subject-designator.T.md")
  'symbol)
