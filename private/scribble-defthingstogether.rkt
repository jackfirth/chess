#lang racket/base

(module doc racket/base
  
  (provide defthingstogether)

  (require scribble/manual
           syntax/parse/define)

  ;@----------------------------------------------------------------------------

  (define-simple-macro
    (defthingstogether (id:id ...) #:contract contract:expr pre-flow ...)
    (deftogether ((defthing id contract) ...) pre-flow ...)))
