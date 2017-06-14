(defpackage :jingoh.documentizer.dsl.spec
  (:use :cl :jingoh :jingoh.documentizer.dsl))
(in-package :jingoh.documentizer.dsl.spec)
(setup :jingoh.documentizer.dsl)

(requirements-about WITH-DOC-DIRECTORY)

;;;; Description:
; make environment to output html.

#+syntax
(WITH-DOC-DIRECTORY (pathname) &body body) ; => result

;;;; Arguments and Values:

; pathname := form generates pathname, otherwise error.
#?(with-doc-directory(0):body) :signals type-error
,:ignore-signals warning

; body := lisp form which print markdown contents.

; result := unspecified.

;;;; Affected By:

;;;; Side-Effects:
; make file on file system.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about WITH-OPEN-MARKDOWN)

;;;; Description:

#+syntax
(WITH-OPEN-MARKDOWN (name) &body body) ; => result

;;;; Arguments and Values:

; name := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about META-DATA-NAME)

;;;; Description:

#+syntax
(META-DATA-NAME sb-kernel:instance) ; => result

#+setf
(SETF (META-DATA-NAME SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about META-DATA-EXPORTS)

;;;; Description:

#+syntax
(META-DATA-EXPORTS sb-kernel:instance) ; => result

#+setf
(SETF (META-DATA-EXPORTS SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about META-DATA-DOC)

;;;; Description:

#+syntax
(META-DATA-DOC sb-kernel:instance) ; => result

#+setf
(SETF (META-DATA-DOC SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about META-DATA-SECTIONS)

;;;; Description:

#+syntax
(META-DATA-SECTIONS sb-kernel:instance) ; => result

#+setf
(SETF (META-DATA-SECTIONS SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about META-DATA-SINGLES)

;;;; Description:

#+syntax
(META-DATA-SINGLES sb-kernel:instance) ; => result

#+setf
(SETF (META-DATA-SINGLES SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about META-DATA-COMMONS)

;;;; Description:

#+syntax
(META-DATA-COMMONS sb-kernel:instance) ; => result

#+setf
(SETF (META-DATA-COMMONS SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about META-DATA-SPECIFIEDS)

;;;; Description:

#+syntax
(META-DATA-SPECIFIEDS sb-kernel:instance) ; => result

#+setf
(SETF (META-DATA-SPECIFIEDS SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MAKE-META-DATA)

;;;; Description:

#+syntax
(MAKE-META-DATA form) ; => result

;;;; Arguments and Values:

; form := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

