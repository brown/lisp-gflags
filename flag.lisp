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
(declaim #.*optimize-default*)

;;; Flag objects hold information needed to parse and set global flags.

(defclass flag ()
  ((name :reader name
         :initarg :name
         :type symbol
         :documentation "Symbol holding this flag's value.")
   (help :reader help
         :initarg :help
         :type string
         :documentation "Help message describing this flag.")
   (parser :reader parser
           :initarg :parser
           :type symbol
           :documentation
"Function designator that names a parser function for the flag.  The parser
takes a string as argument and must return two values, the parsed flag value and
a boolean indicating whether the parse was successful.")
   (type-specifier :reader type-specifier
                   :initarg :type-specifier
                   :type (or symbol cons)
                   :documentation "Type of the flag's value."))
  (:documentation
"A global flag that can be initialized by parsing a command line argument string."))

(defun boolean-flag-p (flag)
  "Returns true if FLAG is a boolean flag; otherwise, returns false."
  (eq (type-specifier flag) 'boolean))

;;; A database of registered flags

(defvar *registered-flags* ()
  "Association list mapping flag selector strings to FLAG instances.")

(defun find-flag (selector)
  "Searches for the registered flag corresponding to the SELECTOR string."
  (cdr (assoc selector *registered-flags* :test #'string=)))

(defun register-flag (selector flag)
  "Stores FLAG in a database of registered flags under key SELECTOR."
  (push (cons selector flag) *registered-flags*))

;;; Parsers that convert strings into basic types.

(defun parse-boolean (string)
  "Parses a STRING representing a boolean value.  Returns two values, the
boolean and a second boolean indicating whether the parse was successful."
  (cond ((or (string= string "true") (string= string "yes")) (values t t))
        ((or (string= string "false") (string= string "no")) (values nil t))
        (t (values nil nil))))

(defun parse-keyword (string)
  "Returns a symbol in the keyword package with the same name as STRING and T to
indicate that parsing STRING was successful."
  (values (intern (string-upcase string) (find-package :keyword)) t))

(defun parse-symbol (string)
  "Returns a symbol in the current package with the same name as STRING and T to
indicate that parsing STRING was successful."
  (values (intern (string-upcase string)) t))

(defun parse-string (string)
  "Returns two values, STRING and T, to indicate that the parse was (trivially)
successful."
  (values string t))

(defun valid-float-characters-p (string float-type)
  "Returns true if every character of STRING is one that may be produced when
printing a FLOAT-TYPE floating point number.  Otherwise, returns false."
  (let ((valid-characters
          (ecase float-type
            ((single-float)
             '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\- #\+ #\e #\E #\f #\F #\Space))
            ((double-float)
             '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\- #\+ #\e #\E #\d #\D #\Space)))))
    (every (lambda (c) (member c valid-characters)) string)))

(defun parse-float (string expected-type)
  "Parses STRING, which represents a floating point value of EXPECTED-TYPE.
Returns two values, the floating point number and a boolean indicating whether
the parse was successful."
  (if (not (valid-float-characters-p string expected-type))
      (values nil nil)
      (with-standard-io-syntax
        (let* ((*read-default-float-format* expected-type)
               (*read-eval* nil)
               (float (ignore-errors (read-from-string string))))
          (if (typep float expected-type)
              (values float t)
              (values nil nil))))))

(defun parse-single-float (string)
  "Parses STRING, which represents a single precision floating point value.
Returns two values, the single-float number and a boolean indicating whether the
parse was successful."
  (parse-float string 'single-float))

(defun parse-double-float (string)
  "Parses STRING, which represents a double precision floating point value.
Returns two values, the double-float number and a boolean indicating whether the
parse was successful."
  (parse-float string 'double-float))

(defun flag-parser (type-spec)
  "Maps TYPE-SPEC, a Lisp type specifier form, into a designator for a function
that can parse the string representation of a TYPE-SPEC value."
  ;; Return a function designator instead of a function because our return value is embedded in
  ;; compiled fasl files when code uses DEFINE-FLAG.
  (cond ((subtypep type-spec 'boolean) 'parse-boolean)
        ((subtypep type-spec 'keyword) 'parse-keyword)
        ((subtypep type-spec 'symbol) 'parse-symbol)
        ((subtypep type-spec 'string) 'parse-string)
        ((subtypep type-spec 'integer) 'parse-integer)
        ((subtypep type-spec 'single-float) 'parse-single-float)
        ((subtypep type-spec 'double-float) 'parse-double-float)
        (t (error "custom flag type ~S but no flag parser specified" type-spec))))

(defun validate-arguments (name default-value-supplied-p selector-supplied-p type-supplied-p
                           selector booleanp)
  "Validates arguments supplied to DEFINE-FLAG."
  (when (not default-value-supplied-p) (error "no default value for flag ~S" selector))
  (when (not selector-supplied-p) (error "no flag selector for flag variable ~S" 'name))
  (when (not type-supplied-p) (error "no type for flag ~S" selector))
  (let ((flag (find-flag selector)))
    (when (and flag (not (eq name (name flag))))
      (error "flag ~S is already defined" selector)))
  ;; Ensure that registering this flag will not cause any ambiguity due to the special handling of
  ;; boolean flags.  Flag arguments "--nobody true" cause boolean flag "body" to be set false, not
  ;; the value of flag "nobody" to be set true.
  (when (and booleanp (find-flag (concatenate 'string "no" selector)))
    (error "boolean flag ~S conflicts with a previously defined flag no~S" selector selector))
  (when (prefixp "no" selector)
    (let* ((without-no (subseq selector 2))
           (without-no-flag (find-flag without-no)))
      (when (and without-no-flag (boolean-flag-p without-no-flag))
        (error "flag ~S conflicts with previously defined boolean flag ~S" selector without-no)))))

;;; Flag defining macro.

(defmacro define-flag (name
                       &key
                         (default-value nil default-value-supplied-p)
                         (selector nil selector-supplied-p)
                         (type nil type-supplied-p)
                         (help "")
                         (parser nil parser-supplied-p)
                         (documentation nil documentation-supplied-p))
  "Defines a global variable NAME with type TYPE, holding value DEFAULT-VALUE
that can be set via the Unix command line to \"value\" with argument
\"--SELECTOR=value\" or argument \"--SELECTOR value\".  As a special case, flags
of type \"boolean\" can additionally be set to true with \"--SELECTOR\" and to
false with \"--noSELECTOR\".

Optionally, associates a HELP string with the flag and a DOCUMENTATION string
with NAME.

Values for flags defined to be of boolean, string, keyword, integer, or floating
point type are parsed by built-in parsers.  For flags of other types supply
PARSER, a function designator for a function that converts a string into a value
of type TYPE.  The parser must return two values, the parsed flag value and a
boolean indicating whether the parse was successful.

Examples:

    (define-flag *debug-flag*
      :default-value nil
      :selector \"debug\"
      :type boolean
      :help \"Turn on debugging mode?\"
      :documentation \"Is debugging mode turned on?\")

    (define-flag *ip-address-flag*
      :default-value (make-instance 'ip-address ...)
      :selector \"ip-address\"
      :type (satisfies ip-address-p)
      :help \"An internet protocol address\")"
  (validate-arguments name default-value-supplied-p selector-supplied-p type-supplied-p
                      selector (eq type 'boolean))
  (let ((documentation (when documentation-supplied-p (list documentation)))
        (parser (if parser-supplied-p parser (flag-parser type))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (declaim (type ,type ,name))
         (defparameter ,name ,default-value ,@documentation))
       (eval-when (:execute :load-toplevel)
         (register-flag ,selector
                        (make-instance 'flag
                                       :name ',name
                                       :help ,help
                                       :parser ',parser
                                       :type-specifier ',type))))))

;;; Command line argument parsing

(defun parse-command-line (arguments)
  "Parses ARGUMENTS, a list of command line argument strings.  If a registered
flag is found in ARGUMENTS, sets the flag's value.  Returns a copy of ARGUMENTS,
but with all recognized flag arguments removed."
  (let ((unknown-arguments ()))
    (loop for argument = (pop arguments) then (pop arguments)
          while argument do
            (if (not (prefixp "--" argument))
                ;; Google flags must start with "--".
                (push argument unknown-arguments)
                ;; Flag values may be set with one argument, "--flag=value" or by passing two
                ;; separate arguments, "--flag value".
                (let* ((equal-sign-index (position #\= argument))
                       (selector (subseq argument 2 equal-sign-index))
                       (flag (find-flag selector))
                       (boolean-value nil))
                  ;; Handle the short forms of boolean flags.  If "boolflag" is a registered
                  ;; boolean flag, then the argument "--boolflag" sets it true and "--noboolflag"
                  ;; sets it false.
                  (unless equal-sign-index
                    (if (and flag (boolean-flag-p flag))
                        (setf boolean-value "true")
                        (when (prefixp "no" selector)
                          (let* ((boolean-selector (subseq selector 2))
                                 (boolean-flag (find-flag boolean-selector)))
                            (when (and boolean-flag (boolean-flag-p boolean-flag))
                              (setf selector boolean-selector)
                              (setf flag boolean-flag)
                              (setf boolean-value "false"))))))
                  (if (not flag)
                      ;; An unregistered flag.
                      (push argument unknown-arguments)
                      ;; Special handling for short boolean flags takes precedence over normal
                      ;; value string extraction.
                      (let ((value-string
                              (cond (boolean-value boolean-value)
                                    (equal-sign-index (subseq argument (1+ equal-sign-index)))
                                    (t (pop arguments)))))
                        (when (null value-string)
                          (error "flag ~S missing value" selector))
                        (multiple-value-bind (value success)
                            (funcall (parser flag) value-string)
                          (unless success
                            (error "cannot convert ~S into a value for flag ~S"
                                   value-string selector))
                          (let ((type (type-specifier flag)))
                            (unless (typep value type)
                              (error "value ~S for flag ~S is not of type ~S"
                                     value selector type)))
                          (setf (symbol-value (name flag)) value)))))))
    (reverse unknown-arguments)))

(defun command-line ()
  "Returns the Unix command line as a list of strings."
  #+ccl (ccl::command-line-arguments)
  #+sbcl sb-ext:*posix-argv*
  #-(or ccl sbcl) (error "not implemented"))
