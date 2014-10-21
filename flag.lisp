;;;; Copyright 2011 Google Inc.  All Rights Reserved

;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are
;;;; met:

;;;;     * Redistributions of source code must retain the above copyright
;;;; notice, this list of conditions and the following disclaimer.
;;;;     * Redistributions in binary form must reproduce the above
;;;; copyright notice, this list of conditions and the following disclaimer
;;;; in the documentation and/or other materials provided with the
;;;; distribution.
;;;;     * Neither the name of Google Inc. nor the names of its
;;;; contributors may be used to endorse or promote products derived from
;;;; this software without specific prior written permission.

;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;; Author: brown@google.com (Robert Brown)

;;;; Unix command line flag parsing.

(in-package #:com.google.flag)

;;; Flag objects hold information needed to parse and set global flags.

(defclass flag ()
  ((flag-variable :reader flag-variable
                  :initarg :flag-variable
                  :type symbol
                  :documentation "Variable holding this flag's value.")
   (help :reader help
         :initarg :help
         :type string
         :documentation "Help message describing this flag.")
   (parser :reader parser
           :initarg :parser
           :type symbol
           :documentation
"Function designator that names a parser function for the flag.  The flag
parser takes a string as argument and must return two values, the parsed flag
value and a boolean indicating whether the parse was successful.")
   (type-specifier :reader type-specifier
                   :initarg :type-specifier
                   :type (or symbol cons)
                   :documentation "Type of the flag's value."))
  (:documentation
"A global flag that can be initialized by parsing a command line argument."))

(defun boolean-flag-p (flag)
  "Returns true if FLAG is a boolean flag; otherwise, returns false."
  (declare (type flag flag))
  (eq (type-specifier flag) 'boolean))

;;; A database of registered flags

(defvar *registered-flags* '()
  "Association list mapping flag selector strings to FLAG instances.")

(defun find-flag (selector)
  "Searches for the registered flag corresponding to the SELECTOR string."
  (declare (type string selector))
  (cdr (assoc selector *registered-flags* :test #'string=)))

(defun validate-flag (flag selector)
  "Validates whether a FLAG associated with SELECTOR can be registered without
causing any flag parsing problems."
  ;; The flag can be redefined so long as its variable does not change.
  (declare (type flag flag)
           (type string selector))
  (let ((old-flag (find-flag selector)))
    (when (and old-flag (not (eq (flag-variable old-flag) (flag-variable flag))))
      (error "a flag with selector ~S is already associated with variable ~S"
             selector (flag-variable old-flag))))
  ;; Ensure that registering this flag will not cause any ambiguity due to the special handling of
  ;; boolean flags.  For example, the flag arguments "--nobody true" cause boolean flag "body" to
  ;; be set false, not the value of flag "nobody" to be set true.
  (when (boolean-flag-p flag)
    (let ((old-flag (find-flag (concatenate 'string "no" selector))))
      (when old-flag
        (error "selector for flag variable ~S conflicts with the selector for flag variable ~S"
               (flag-variable flag) (flag-variable old-flag)))))
  (when (prefixp "no" selector)
    (let* ((without-no (subseq selector 2))
           (without-no-flag (find-flag without-no)))
      (when (and without-no-flag (boolean-flag-p without-no-flag))
        (error "selector for flag variable ~S conflicts with that of boolean flag variable ~S"
               (flag-variable flag) (flag-variable without-no-flag))))))

(defun register-flag (flag selector)
  "Stores FLAG in a database of registered flags, indexing it by SELECTOR."
  (declare (type flag flag)
           (type string selector))
  (validate-flag flag selector)
  (when (find-flag selector)
    (setf *registered-flags* (remove selector *registered-flags* :key #'car :test #'string=)))
  (push (cons selector flag) *registered-flags*))

;;; Parsers that convert strings into basic types.

(defun parse-boolean (string)
  "Parses a STRING representing a boolean value.  Returns two values, the
boolean and a second boolean indicating whether the parse was successful."
  (declare (type string string))
  (cond ((string= string "true") (values t t))
        ((string= string "false") (values nil t))
        (t (values nil nil))))

(defun parse-keyword (string)
  "Returns a symbol in the keyword package with the same name as STRING and T to
indicate that parsing STRING was successful."
  (declare (type string string))
  (values (intern (string-upcase string) (find-package :keyword)) t))

(defun parse-symbol (string)
  "If STRING, converted to upper case, represents a package-qualified symbol,
returns two values, the symbol and T.  Otherwise, returns NIL and NIL."
  (declare (type string string))
  (let ((first-colon (position #\: string))
        (last-colon (position #\: string :from-end t)))
    (unless (and first-colon
                 ;; Require a non-empty package name.  Most Lisps return NIL for (find-package ""),
                 ;; but Allegro returns the keyword package.
                 (plusp first-colon)
                 (or (= first-colon last-colon) (= (1+ first-colon) last-colon)))
      (return-from parse-symbol (values nil nil)))
    (let ((package (find-package (string-upcase (subseq string 0 first-colon)))))
      (if package
          (values (intern (string-upcase (subseq string (1+ last-colon))) package) t)
          (values nil nil)))))

(defun parse-string (string)
  "Returns two values, STRING and T, to indicate that the parse was (trivially)
successful."
  (declare (type string string))
  (values string t))

(defun valid-float-characters-p (string float-type)
  "Returns true if every character of STRING is one that may be produced when
printing a FLOAT-TYPE floating point number.  Otherwise, returns false."
  (declare (type string string)
           (type (member single-float double-float) float-type))
  (let ((valid-characters
          (ecase float-type
            ((single-float) "0123456789.-+eEfF")
            ((double-float) "0123456789.-+eEdD"))))
    (every (lambda (c) (position c valid-characters)) string)))

(defun parse-float (string expected-type)
  "Parses STRING, which represents a floating point value of EXPECTED-TYPE.
Returns two values, the floating point number and a boolean indicating whether
the parse was successful."
  (declare (type string string)
           (type (member single-float double-float) expected-type))
  (if (not (valid-float-characters-p string expected-type))
      (values nil nil)
      (let ((float (with-standard-io-syntax
                     (let ((*read-default-float-format* expected-type)
                           (*read-eval* nil))
                       (ignore-errors (read-from-string string))))))
        (if (typep float expected-type)
            (values float t)
            (values nil nil)))))

(defun parse-single-float (string)
  "Parses STRING, which represents a single precision floating point value.
Returns two values, the single-float number and a boolean indicating whether
the parse was successful."
  (declare (type string string))
  (parse-float string 'single-float))

(defun parse-double-float (string)
  "Parses STRING, which represents a double precision floating point value.
Returns two values, the double-float number and a boolean indicating whether
the parse was successful."
  (declare (type string string))
  (parse-float string 'double-float))

(defun flag-parser (type-specifier)
  "Maps a Lisp TYPE-SPECIFIER form, into a designator for a function that can
parse the string representation of a TYPE-SPECIFIER value.  Returns NIL when
there is no predefined parser for TYPE-SPECIFIER."
  (declare (type (or symbol cons) type-specifier))
  ;; Return a symbol because our return value is embedded in the compiled FASLs of files that
  ;; contain a DEFINE-FLAG form.  Functions cannot be embedded in FASLs, but symbols can.
  (cond ((subtypep type-specifier 'null) nil)
        ((subtypep type-specifier 'boolean) 'parse-boolean)
        ((subtypep type-specifier '(or null keyword)) 'parse-keyword)
        ((subtypep type-specifier 'symbol) 'parse-symbol)
        ((subtypep type-specifier '(or null string)) 'parse-string)
        ((subtypep type-specifier '(or null integer)) 'parse-integer)
        ((subtypep type-specifier '(or null single-float)) 'parse-single-float)
        ((subtypep type-specifier '(or null double-float)) 'parse-double-float)))

;;; Flag defining macro.

(defmacro define-flag (flag-variable
                       &key
                         (default-value nil default-value-supplied-p)
                         (selector nil selector-supplied-p)
                         (type nil type-supplied-p)
                         (help "")
                         (parser nil parser-supplied-p)
                         (documentation nil documentation-supplied-p))
  "Defines a global FLAG-VARIABLE of type TYPE, holding value DEFAULT-VALUE,
that can be set via the Unix command line to \"value\" with argument
\"--SELECTOR=value\" or arguments \"--SELECTOR value\".  As a special case,
flags of type \"boolean\" can additionally be set to true with \"--SELECTOR\"
and to false with \"--noSELECTOR\".

Optionally, associates a HELP string with the flag and a DOCUMENTATION string
with FLAG-VARIABLE.

Values for flags defined to be of boolean, string, keyword, integer, or
floating point type are parsed by built-in parsers.  For flags of other types
supply PARSER, a function designator for a function that converts a string into
a value of type TYPE.  The parser must return two values, the parsed flag value
and a boolean indicating whether the parse was successful.

Examples:

    (define-flag *debug-mode*
      :default-value nil
      :selector \"debug\"
      :type boolean
      :help \"Turn on debugging mode?\"
      :documentation \"Is debugging mode turned on?\")

    (define-flag *ip-address*
      :default-value (make-instance 'ip-address ...)
      :selector \"ip_address\"
      :type (satisfies ip-address-p)
      :parser ip-address-parser
      :help \"An internet protocol address.\")"
  (assert (symbolp flag-variable)
          (flag-variable) "flag variable ~S is not a symbol" flag-variable)
  (assert default-value-supplied-p
          (flag-variable) "no default value specified for flag variable ~S" flag-variable)
  (assert selector-supplied-p
          (selector) "no selector for flag variable ~S" flag-variable)
  (assert (stringp selector)
          (selector) "selector ~S for flag variable ~S is not a string" selector flag-variable)
  (assert (not (string= selector ""))
          (selector) "empty selector string for flag variable ~S" flag-variable)
  (assert type-supplied-p
          (type) "no type specified for flag variable ~S" flag-variable)
  (assert (or (symbolp type) (consp type))
          (type) "invalid type specified for flag variable ~S " flag-variable)
  (assert (stringp help)
          (help) "help for flag variable ~S is not a string" flag-variable)
  (assert (symbolp parser)
          (parser) "parser for flag variable ~S is not a symbol" flag-variable)
  (assert (or (not documentation-supplied-p) (stringp documentation))
          (documentation) "documentation for flag variable ~S is not a string" flag-variable)

  (let ((documentation (when documentation-supplied-p (list documentation)))
        (parser (if parser-supplied-p parser (flag-parser type))))
    (assert parser
            (parser) "no parser defined for flag variable ~S" flag-variable)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (declaim (type ,type ,flag-variable))
         (defparameter ,flag-variable ,default-value ,@documentation))
       (eval-when (:execute :load-toplevel)
         (register-flag (make-instance 'flag
                                       :flag-variable ',flag-variable
                                       :help ,help
                                       :parser ',parser
                                       :type-specifier ',type)
                        ,selector))
       ',flag-variable)))

;;; Command line argument parsing

(defun parse-command-line (arguments)
  "Parses ARGUMENTS, a list of command line argument strings.  If a registered
flag is found in ARGUMENTS, sets the flag's value.  Returns a copy of
ARGUMENTS, but with all recognized flag arguments removed."
  (declare (type list arguments))
  (let ((unrecognized-arguments '()))
    (loop while arguments do
      (let ((argument (pop arguments)))
        (declare (type string argument))
        (cond ((string= argument "--")
               ;; The special flag "--" marks the end of parsed arguments.
               (return-from parse-command-line
                 (append (reverse unrecognized-arguments) arguments)))
              ((not (prefixp "--" argument))
               ;; Google flags must start with "--".
               (push argument unrecognized-arguments))
              (t
               ;; Flag values may be set with one argument, "--flag=value" or by passing two
               ;; separate arguments, "--flag value".
               (let* ((equal-sign-index (position #\= argument))
                      (selector (subseq argument 2 equal-sign-index))
                      (flag (find-flag selector))
                      (boolean-value nil))
                 (declare (type string selector))
                 ;; Handle the short forms of boolean flags.  If "boolflag" is a registered boolean
                 ;; flag, then the argument "--boolflag" sets it true and "--noboolflag" sets it
                 ;; false.  If either of these forms is used, the following argument must be
                 ;; another flag to avoid the "--boolflag false" trap.
                 (unless equal-sign-index
                   (if (and flag (boolean-flag-p flag))
                       (setf boolean-value "true")
                       (when (prefixp "no" selector)
                         (let* ((boolean-selector (subseq selector 2))
                                (boolean-flag (find-flag boolean-selector)))
                           (when (and boolean-flag (boolean-flag-p boolean-flag))
                             (setf selector boolean-selector)
                             (setf flag boolean-flag)
                             (setf boolean-value "false")))))
                   (when (and boolean-value arguments (not (prefixp "--" (first arguments))))
                     (error "short form of boolean flag must be followed by another flag")))
                 (if (not flag)
                     ;; An unregistered flag.
                     (push argument unrecognized-arguments)
                     ;; Special handling for short boolean flags takes precedence over normal
                     ;; value string extraction.
                     (let ((value-string
                             (cond (boolean-value boolean-value)
                                   (equal-sign-index (subseq argument (1+ equal-sign-index)))
                                   (t (pop arguments)))))
                       (unless value-string (error "flag selector ~S missing value" selector))
                       (multiple-value-bind (value success)
                           (funcall (fdefinition (parser flag)) value-string)
                         (unless success
                           (error "cannot convert ~S into a value for flag ~S"
                                  value-string selector))
                         (let ((type (type-specifier flag)))
                           (unless (typep value type)
                             (error "value ~S for flag ~S is not of type ~S" value selector type)))
                         (setf (symbol-value (flag-variable flag)) value)))))))))
    (reverse unrecognized-arguments)))

(defun command-line ()
  "Returns the Unix command line as a list of strings."
  #+allegro (sys:command-line-arguments)
  #+ccl (ccl::command-line-arguments)
  #+sbcl sb-ext:*posix-argv*
  #-(or allegro ccl sbcl) (error "not implemented"))
