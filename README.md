Reference Document Generator for Coalton

This system can be used to generate reference sheets for Coalton
libraries.  Right now it covers types, classes, and values, but
eventually I want to add something for instances and native Lisp
functions and macros.

By default it outputs markdown, but with `(setf *format* :html)` you
can create docs for the web.

[Here](https://github.com/garlic0x1/coalton-threads/blob/master/REFERENCE.md)
is a sample document generated with the following code:

```lisp
(render-docs *standard-output* :coalton-threads)
```