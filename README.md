# Lisp gflags

Lisp gflags is a Common Lisp implementation of gflags, Google's command line
flag parsing library.  The library implements functionality similar to that of
Google's [C++ gflags library](http://github.com/gflags/gflags/) and [Python
gflags library](http://github.com/abseil/abseil-py/).

The code allows you to define command line flags using the `DEFINE-FLAG` macro.
For instance, the following form defines a boolean flag called `*DEBUG-FLAG*`
that is set based on the presence of "--debug" in the application's command
line:

```
(define-flag *debug-flag*
  :default-value nil
  :selector "debug"
  :type boolean
  :help "Turn on debugging mode?"
  :documentation "Is debugging mode turned on?")
```

If the command line contains "--debug" or "--debug=true", then `*DEBUG-FLAG*` is
set to `T`.  Otherwise, it defaults to `NIL`.

The function `PARSE-COMMAND-LINE` is used to parse the command line and
initialize flags.  It returns the original command line with all recognized
flags removed.

The file `flag-test.lisp` contains many examples.

## The Lisp gflags API

#### define-flag flag-variable &key default-value selector type help parser documentation

```
Defines a global FLAG-VARIABLE of type TYPE, holding value DEFAULT-VALUE,
that can be set via the Unix command line to "value" with argument
"--SELECTOR=value" or arguments "--SELECTOR value".  As a special case,
flags of type "boolean" can additionally be set to true with "--SELECTOR"
and to false with "--noSELECTOR".

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
      :selector "debug"
      :type boolean
      :help "Turn on debugging mode?"
      :documentation "Is debugging mode turned on?")

    (define-flag *ip-address*
      :default-value (make-instance 'ip-address ...)
      :selector "ip_address"
      :type (satisfies ip-address-p)
      :parser ip-address-parser
      :help "An internet protocol address.")
```

#### parse-command-line arguments

```
Parses ARGUMENTS, a list of command line argument strings.  If a registered
flag is found in ARGUMENTS, sets the flag's value.  Returns a copy of
ARGUMENTS, but with all recognized flag arguments removed.
```

#### command-line

```
Returns the Unix command line as a list of strings.
```
