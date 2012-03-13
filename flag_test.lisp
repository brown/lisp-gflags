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

;;;; Test command line argument flag parsing.

(in-package #:common-lisp-user)

(defpackage #:com.google.flag-test
  (:documentation "Test command line flag parsing code in the COM.GOOGLE.FLAG package.")
  (:use #:common-lisp
        #:com.google.base
        #:com.google.flag
        #:hu.dwim.stefil)
  (:export #:test-flag))

(in-package #:com.google.flag-test)
(declaim #.*optimize-default*)

(defsuite (test-flag :in root-suite) ()
  (run-child-tests))

(in-suite test-flag)

(defconst +before-junk+ '("before" "-f" "-" "--" "--no" ""))
(defconst +after-junk+ '("after" "--" "" "--foo"))
(defconst +junk+ (append +before-junk+ +after-junk+))

(defun parse-flags (arguments)
  (is (equal (parse-command-line (append +before-junk+ arguments +after-junk+)) +junk+)))

(define-flag *boolean-flag*
    :default-value nil
    :selector "boolflag"
    :type boolean
    :help "A boolean flag."
    :documentation "Boolean flag used for testing.")

(deftest boolean-flag ()
  (is (not *boolean-flag*))
  (is (string= (documentation '*boolean-flag* 'variable) "Boolean flag used for testing."))
  (let ((*boolean-flag* *boolean-flag*))
    (parse-flags '("--boolflag"))
    (is *boolean-flag*)
    (setf *boolean-flag* nil)
    (parse-flags '("--boolflag=true"))
    (is *boolean-flag*)
    (setf *boolean-flag* t)
    (parse-flags '("--noboolflag"))
    (is (not *boolean-flag*))
    (setf *boolean-flag* t)
    (parse-flags '("--boolflag=no"))
    (is (not *boolean-flag*))))

(define-flag *keyword-flag* :default-value :foo :selector "keyflag" :type keyword)

(deftest keyword-flag ()
  (is (eq *keyword-flag* :foo))
  (let ((*keyword-flag* *keyword-flag*))
    (parse-flags '("--keyflag" "dog"))
    (is (eq *keyword-flag* :dog))
    (parse-flags '("--keyflag=cat"))
    (is (eq *keyword-flag* :cat))))

(define-flag *symbol-flag* :default-value :bar :selector "symflag" :type symbol)

(deftest symbol-flag ()
  (is (eq *symbol-flag* :bar))
  (let ((*symbol-flag* *symbol-flag*))
    (parse-flags '("--symflag" "com.google.flag:define-flag"))
    (is (eq *symbol-flag* 'com.google.flag:define-flag))
    (parse-flags '("--symflag=com.google.flag-test::unexported-symbol"))
    (is (eq *symbol-flag* 'com.google.flag-test::unexported-symbol))))

(define-flag *string-flag* :default-value "foo" :selector "stringflag" :type string)

(deftest string-flag ()
  (is (string= *string-flag* "foo"))
  (let ((*string-flag* *string-flag*))
    (parse-flags '("--stringflag" "dog"))
    (is (string= *string-flag* "dog"))
    (parse-flags '("--stringflag=cat"))
    (is (string= *string-flag* "cat"))))

(define-flag *integer-flag* :default-value 10  :selector "intflag" :type (integer -10 10))

(deftest integer-flag ()
  (is (= *integer-flag* 10))
  (let ((*integer-flag* *integer-flag*))
    (parse-flags '("--intflag" "-2"))
    (is (= *integer-flag* -2))
    (parse-flags '("--intflag=3"))
    (is (= *integer-flag* 3))))

(define-flag *single-float-flag* :default-value 3.14f0 :selector "sfflag" :type single-float)

(deftest single-float-flag ()
  (is (= *single-float-flag* 3.14f0))
  (let ((*single-float-flag* *single-float-flag*))
    (parse-flags '("--sfflag" "0.42"))
    (is (= *single-float-flag* 0.42f0))
    (parse-flags '("--sfflag=-.42e-2"))
    (is (= *single-float-flag* -.42f-2))))

(define-flag *double-float-flag*
    :default-value 0.12345d0
    :selector "dfflag"
    :type (double-float -1d0 1d0))

(deftest double-float-flag ()
  (is (= *double-float-flag* 0.12345d0))
  (let ((*double-float-flag* *double-float-flag*))
    (parse-flags '("--dfflag" "0.42"))
    (is (= *double-float-flag* 0.42d0))
    (parse-flags '("--dfflag=-.42E-2"))
    (is (= *double-float-flag* -0.42d-2))))

(deftype color () '(member :red :green :blue))

(defun color-parser (string)
  (let ((color (cond ((string= string "red") :red)
                     ((string= string "green") :green)
                     ((string= string "blue") :blue))))
    (if color
        (values color t)
        (values nil nil))))

(define-flag *color* :default-value :red :selector "color" :type color :parser color-parser)

(deftest color-flag ()
  (is (eq *color* :red))
  (let ((*color* *color*))
    (parse-flags '("--color" "blue"))
    (is (eq *color* :blue))))
