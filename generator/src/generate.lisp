(in-package #:jingoh.generator)

;;;; GENERATOR

(declaim
 (ftype (function * (values (function nil (values null &optional)) &optional))
        asd-generator
        readme-generator
        method-extension-appender
        test-asd-generator
        cl-source-file-generator
        readme-updator))

;;; ASD

(defun asd-generator (system-name)
  (lambda ()
    (let ((*package* (find-package :asdf)))
      (format t "; vim: ft=lisp et~%~
              (in-package :asdf)~%~
              ~(~S~)~%"
              `(asdf:defsystem ,system-name
                 :version
                 "0.0.0"
                 :depends-on
                 nil
                 :pathname
                 "src/"
                 :components
                 ((:file ,system-name)))))))

;;; README

;; For future refactoring we dare to write this.
(defun system-version (system)
  (#.(if (uiop:version<= "3.3.2.11" (asdf:asdf-version))
         'asdf:system-version
         'asdf:component-version)
   system))

(defun readme-generator (system-name)
  (let ((system (asdf:find-system system-name nil)))
    (lambda ()
      (format t "# ~@:(~A~) ~A~%~
	      ## What is this?~%~
	      ~@[~A~]~%~
	      ### Current lisp world~2%~
	      ### Issues~2%~
	      ### Proposal~2%~
	      ## Usage~2%~
	      ## From developer~2%~
	      ### Product's goal~2%~
	      ### License~%~
	      ~@[~A~]~%~
	      ### Developed with~2%~
	      ### Tested with~2%~
	      ## Installation~2%"
              system-name
              (or (and system (system-version system)) "0.0.0")
              (and system (asdf:system-description system))
              (and system (asdf:system-license system))))))

;;; METHOD-EXTENSION

(defun method-extension-appender (name)
  (lambda ()
    (let ((*package* (find-package :asdf)) (*print-case* :downcase))
      (format t
              "~%;;; These forms below are added by JINGOH.GENERATOR.~{~%~A~%~S~}"
              `(";; Ensure in ASDF for pretty printings." (in-package :asdf)
                ,(format nil ";; Enable testing via (asdf:test-system ~S)."
                         name)
                (defmethod asdf:component-depends-on
                           ((asdf::o asdf:test-op)
                            (asdf::c (eql (asdf:find-system ,name))))
                  (append (call-next-method)
                          '((asdf:test-op ,(test-name name)))))
                ";; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM."
                (defmethod asdf:operate :around
                           ((asdf::o asdf:test-op)
                            (asdf::c (eql (asdf:find-system ,name)))
                            &rest asdf::keys
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
                  (flet ((asdf::jingoh.args (asdf::keys)
                           (loop :for (asdf::key asdf::value) :on asdf::keys
                                      :by #'cddr
                                 :when (find asdf::key
                                             '(:on-fails :subject :vivid)
                                             :test #'eq)
                                   :collect asdf::key
                                   :and :collect asdf::value
                                 :else :when (eq :jingoh.verbose asdf::key)
                                   :collect :verbose
                                   :and :collect asdf::value)))
                    (let ((asdf::args (asdf::jingoh.args asdf::keys)))
                      (declare (special asdf::args))
                      (call-next-method))))
                ";; Enable importing spec documentations."
                (let ((asdf::system
                       (asdf:find-system "jingoh.documentizer" nil)))
                  (when asdf::system
                    (asdf:load-system asdf::system)
                    (defmethod asdf:perform :after
                               ((asdf::o asdf:load-op)
                                (asdf::c (eql (asdf:find-system ,name))))
                      (uiop:with-muffled-conditions (uiop:*uninteresting-conditions*)
                        (handler-case
                            (uiop:symbol-call :jingoh.documentizer
                                              :import asdf::c)
                          (error (condition)
                            (warn "Fails to import documentation of ~S.~%~A"
                                  (asdf:coerce-name asdf::c)
                                  (princ-to-string condition)))))))))))))

;;; TEST-ASD

(defun test-asd-generator (system forms)
  (lambda ()
    (labels ((component (form)
               `(:file ,(string-downcase (second form))))
             (examine-form (form)
               `(apply #'uiop:symbol-call :jingoh :examine
                       ,(package-key (second form)) asdf::args))
             (package-key (package-name)
               (intern (string package-name) :keyword)))
      (let ((*package* (find-package :asdf)))
        (format t "; vim: ft=lisp et~%~
		(in-package :asdf)~%~
		~(~S~)"
                `(asdf:defsystem ,(test-name system)
                   :version
                   "0.0.0"
                   :depends-on
                   (:jingoh ,(asdf:coerce-name system))
                   :components
                   ,(mapcar #'component forms)
                   :perform
                   (asdf:test-op (asdf::o asdf::c)
                                 (declare (special asdf::args))
                                 ,@(mapcar #'examine-form forms))))))))

;;; CL-SOURCE-FILE

(defun cl-source-file-generator (system-name)
  (let ((package-name (intern (string-upcase system-name) :keyword)))
    (lambda ()
      (format t "~(~S~)~%~(~S~)~%~(~S~)~2%" '(in-package :cl-user)
              `(defpackage ,package-name
                 (:use :cl)
                 (:export))
              `(in-package ,package-name)))))

;;; README-UPDATOR

(defun readme-updator (system readme-lines)
  (lambda ()
    (format t "# ~@:(~A~) ~:[0.0.0~;~:*~A~]~%" (asdf:coerce-name system)
            (system-version system))
    (do* ((lines (cdr readme-lines) (cdr lines))
          (line (car lines) (car lines)))
         ((endp lines) (force-output))
      (flet ((skip-to (prefix)
               (setf lines
                       (nthcdr
                         (position-if
                           (lambda (line) (uiop:string-prefix-p prefix line))
                           (cdr lines))
                         lines))))
        (cond
         ((uiop:string-prefix-p "## What is this?" line)
          (format t "~A~%~:[~;~:*~A~]~2%" line
                  (asdf:system-description system))
          (skip-to "##"))
         ((uiop:string-prefix-p "### License" line)
          (format t "~A~%~:[TODO~;~:*~A~]~2%" line
                  (asdf:system-license system))
          (skip-to "##"))
         (t (write-line line)))))))

;;;; GENERATE
;;; SYMBOL

(defmethod generate ((symbol symbol) &key system init pathname append)
  (if (keywordp symbol)
      (if init
          (generate 'init :system symbol :pathname pathname)
          (generate (asdf:find-system symbol) :append append))
      (let* ((*package* (symbol-package symbol))
             (package-name (package-name *package*))
             (system
              (asdf:find-system (or system (string-downcase package-name))))
             (*default-pathname-defaults* (spec-directory system))
             (forms
              `((defpackage ,package-name
                  (:export ,symbol))))
             (path (path-of (test-name system) "asd")))
        (unless (probe-file path)
          (generate 'test-asd :system system :forms forms :path path))
        (dolist (form forms) (generate form :append t)))))

;;; INIT
#| Generate project skelton.
   project/---
            |---project.asd
            |---README.md
            |---src/---
                     |---project.lisp
|#

(defmethod generate ((dispatcher (eql 'init)) &key system pathname)
  (let* ((system-name (ensure-name system))
         (*default-pathname-defaults*
          (uiop:subpathname
            (or (and pathname (uiop:ensure-directory-pathname pathname))
                (local-project-directory))
            (uiop:ensure-directory-pathname system-name))))
    (output-to (path-of system-name "asd") (asd-generator system-name))
    (output-to (path-of "README" "md") (readme-generator system-name))
    (output-to
      (path-of system-name "lisp"
               (uiop:subpathname *default-pathname-defaults* "src/"))
      (cl-source-file-generator system-name))
    (ql:register-local-projects)
    (when (find-package :roswell)
      (uiop:symbol-call :roswell.util "LOCAL-PROJECT-BUILD-HASH" :rebuild t))
    (generate (asdf:find-system system-name))))

(defun ensure-name (system)
  (let ((name (asdf:coerce-name system)))
    (if (not
          (find name (ql-dist:provided-systems t)
                :key #'ql-dist:name
                :test #'equal))
        name
        (restart-case (error "~S is already used in quicklisp." name)
          (continue () :report "Use it anyway." name)
          (rename (name)
              :report "Specify new name."
              :interactive (lambda ()
                             (format *query-io* ">> ")
                             (force-output *query-io*)
                             (list (read-line *query-io*)))
            (ensure-name name))))))

;;; LOCAL-PROJECT-DIRECTORY

(defun find-asd (pathname)
  (uiop:collect-sub*directories pathname #'uiop:directory-exists-p
                                #'uiop:directory-exists-p
                                (lambda (directory)
                                  (let ((files
                                         (uiop:directory-files directory
                                                               "*.asd")))
                                    (when files
                                      (return-from find-asd files))))))

(defun parse-registry (source-registry)
  (mapcan
    (lambda (directive)
      (when (typep directive '(cons (member :directory :tree) *))
        (list (uiop:resolve-location (cadr directive)))))
    (cdr source-registry)))

(defun default-source-registry-directories ()
  (mapcan (lambda (symbol) (parse-registry (funcall symbol)))
          asdf:*default-source-registries*))

(defun asdf-default-source-registries-directories ()
  (loop :for directory :in (default-source-registry-directories)
        :when (find-asd directory)
          :collect directory))

(define-condition some-repository (simple-error) ())

(defun local-project-directory ()
  (let ((directories
         (append (asdf-default-source-registries-directories)
                 ql:*local-project-directories*
                 (when (find-package :roswell)
                   (symbol-value
                     (uiop:find-symbol* "*LOCAL-PROJECT-DIRECTORIES*"
                                        :roswell))))))
    (if (typep directories '(cons * null))
        (car directories)
        (restart-case (error
                        (if directories
                            'some-repository
                            'simple-error)
                        :format-control "Could not determine to configure.")
          (select (index)
              :report "Select directory from knwon list."
              :test (lambda (condition) (typep condition 'some-repository))
              :interactive (lambda ()
                             (loop :for d :in directories
                                   :for i :upfrom 0
                                   :do (format *query-io* "~%~3D: ~S" i d))
                             (list
                               (prompt-for:prompt-for
                                 `(mod ,(length directories))
                                 "~%Which directory do you use?~%Please type number >> ")))
            (nth index directories))
          (use-value (directory)
              :report "Specify directory."
              :interactive (lambda ()
                             (let* ((input
                                     (prompt-for:prompt-for 'string
                                                            "Specify >> "
                                                            :by #'read-line))
                                    (exists? (uiop:directory-exists-p input)))
                               (list
                                 (or exists?
                                     (restart-case (error
                                                     "Directory does not exist.")
                                       (make ()
                                           :report "Make directory then continue."
                                         (ensure-directories-exist
                                           (uiop:ensure-directory-pathname
                                             input))))))))
            directory)))))

;;; SYSTEM

(defmethod generate ((system asdf:system) &key append)
  (let* ((forms)
         (*macroexpand-hook*
          (let ((outer-hook *macroexpand-hook*))
            (lambda (expander form env)
              (when (typep form '(cons (eql defpackage) *))
                (pushnew form forms :key #'second :test #'string=))
              (funcall outer-hook expander form env))))
         (*default-pathname-defaults* (spec-directory system))
         (test-asd-path (path-of (test-name system) "asd")))
    (mapc #'asdf:load-system (asdf:system-depends-on system))
    (asdf:load-system system :force t)
    ;; In order to generate asd for already existing project,
    ;; and avoid generate extension duplicatedly,
    ;; adding extension unless spec dir exists.
    (unless (probe-file test-asd-path)
      (add-method-extension system))
    (generate 'test-asd :system system :forms forms :path test-asd-path)
    (dolist (form forms) (generate form :append append)))
  (ql:register-local-projects)
  (when (find-package :roswell)
    (uiop:symbol-call :roswell.util "LOCAL-PROJECT-BUILD-HASH" :rebuild t)))

(defun add-method-extension (system)
  (let ((system (asdf:find-system system)))
    (output-to (asdf:system-source-file system)
               (method-extension-appender (asdf:coerce-name system))
               :if-exists :append)))

;;; TEST-ASD

(defmethod generate ((dispatcher (eql 'test-asd)) &key system forms path)
  (ensure-directories-exist *default-pathname-defaults*)
  (output-to path (test-asd-generator system forms)))

;;; DEFPACKAGE-FORM

(defmethod generate ((form list) &key append)
  (assert (typep form '(cons (eql defpackage) t)))
  (labels ((exports (form)
             (loop :for elt :in form
                   :when (typep elt '(cons (eql :export) t))
                     :append (cdr elt))))
    (let* ((path (path-of (string-downcase (second form)) "lisp"))
           (*package* (find-package (second form)))
           (existp (probe-file path)))
      (output-to path
                 (lambda ()
                   (unless existp
                     (generate-header (second form)))
                   (dolist (symbol (exports form))
                     (symbol-generate symbol (second form))))
                 :if-exists (if (and existp append)
                                :append
                                :error)))))

(defun generate-header (package-name)
  (let ((spec-name (intern (format nil "~A.SPEC" package-name) :keyword)))
    (format t "~(~S~)~%~
	    (in-package ~(~S~))~%~
	    (setup ~(~S~))~2%"
            `(defpackage ,spec-name
               (:use :cl :jingoh ,package-name))
            spec-name (intern (string package-name) :keyword))))

;;; :README

(defmethod generate ((dispatcher (eql :readme)) &key system)
  (declare (ignore dispatcher))
  (setf system (asdf:find-system system)) ; as canonicalize.
  (let* ((readme-path
          (path-of "README" "md" (asdf:system-source-directory system)))
         (existp (probe-file readme-path))
         (lines (and existp (uiop:read-file-lines readme-path))))
    (output-to readme-path
               (if (or existp lines)
                   (readme-updator system lines)
                   (readme-generator (asdf:coerce-name system))))))