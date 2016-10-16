(in-package :jingoh.org)

(defstruct(org (:copier nil))
  #.(doc :jingoh.org "doc/org.T.md")
  (name nil :type symbol :read-only t)
  (current-subject nil :type symbol)
  (options nil :type list)
  (specifications (make-array 0 :fill-pointer 0 :adjustable t) :type vector))

(defmethod print-object((o org)*standard-output*)
  (if *print-escape*
    (print-unreadable-object(o *standard-output* :type t)
      (let((count(org-requirements-count o)))
	(format t "~A~:[ ~D ~:*requirement~P~;~]"
		(org-name o)
		(zerop count)
		count)))
    (call-next-method)))

(deftype org-designator()
  #.(doc :jingoh.org "doc/org-designator.T.md")
  '(or (and symbol (not boolean))
       org))

(deftype subject-designator()
  #.(doc :jingoh.org "doc/subject-designator.T.md")
  'symbol)
